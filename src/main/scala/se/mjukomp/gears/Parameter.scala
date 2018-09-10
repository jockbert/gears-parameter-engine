package se.mjukomp.gears

import Parameter._
import scala.annotation.tailrec

object Parameter {
  type Value = Double
  type MonotoneFn = Value => Value

  val valueMin: Value = Double.MinValue
  val valueMax: Value = Double.MaxValue
  val defaultRange = Range(valueMin, valueMax)

  def apply(name: String, value: Value): Parameter =
    apply(name, value, valueMin, valueMax)
}

case class Range(min: Value, max: Value) {

  def intersection(other: Range) =
    if (other.max < min || other.min > max) None
    else Some(Range(
      Math.min(min, other.min),
      Math.max(max, other.max)))

  def map(fn: MonotoneFn) =
    Range(fn(min), fn(max))
}

sealed trait RelationError
case object NoRangeOverlap extends RelationError

case class Relation(from: Parameter, fn: MonotoneFn, to: Parameter) {
  override def toString(): String = s"Relation(${from.name},${to.name})"
}

case class Parameter(
  name:               String,
  private var _value: Value,
  var min:            Value,
  var max:            Value) {

  private var relationsFrom: List[Relation] = Nil
  private var relationsTo: List[Relation] = Nil

  def value() = _value

  @tailrec
  final def backtrackValue(target: Value, relation: MonotoneFn, rangeMin: Value, rangeMax: Value): Double = {

    //System.err.println(s"[$rangeMin, $rangeMax]")
    if (rangeMin + math.ulp(rangeMin) >= rangeMax) {
      if (relation(rangeMin) < target) rangeMax else rangeMin
    } else {
      val rangeMiddle = (rangeMin + rangeMax) / 2
      val middleIsLarge = relation(rangeMiddle) > target

      val newMin = if (middleIsLarge) rangeMin else rangeMiddle
      val newMax = if (middleIsLarge) rangeMiddle else rangeMax

      backtrackValue(target, relation, newMin, newMax)
    }
  }

  def value(v: Value): Unit = {

    relationsFrom.foreach(b => {
      val newValue = backtrackValue(v, b.fn, b.from.min, b.from.max)
      b.from.value(newValue)
    })

    _value = v

    relationsTo.foreach(b => b.to._value = b.fn(v))
  }

  def functionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Relation] = {

    _value = fn(source.value)

    val otherRange = Range(source.min, source.max)
    val range = Range(min, max)
    val mappedRange = otherRange.map(fn)
    val narrowedRange = mappedRange.intersection(range)

    if (narrowedRange.isEmpty)
      Left(NoRangeOverlap)
    else {

      val newMin = fn(source.min)
      val isNewMinTooSmall = newMin < min
      if (isNewMinTooSmall) {
        val otherNewMin = backtrackValue(min, fn, source.min, source.max)
        source.min = otherNewMin
      } else {
        min = newMin
      }

      val newMax = fn(source.max)
      val isNewMaxTooLarge = newMax > max
      if (isNewMaxTooLarge) {
        val otherNewMax = backtrackValue(max, fn, source.min, source.max)
        source.max = otherNewMax
      } else {
        max = newMax
      }

      val relation = Relation(source, fn, this)

      relationsFrom = relation :: relationsFrom
      source.relationsTo = relation :: source.relationsTo
      Right(relation)
    }
  }

  def inverseFunctionOf(source: Parameter, fn: MonotoneFn) = {

    min = fn(source.max)
    _value = fn(source.value)
    max = fn(source.min)

    relationsFrom = Relation(source, fn, this) :: relationsFrom
  }
}
