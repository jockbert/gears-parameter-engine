package se.mjukomp.gears

import Parameter._
import scala.annotation.tailrec

object Parameter {
  type Value = Double
  type MonotoneFn = Value => Value

  val ValueMin = Double.MinValue
  val ValueMax = Double.MaxValue

  def apply(name: String, value: Value): Parameter =
    apply(name, ValueMin, value, ValueMax)

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

case class Binding(from: Parameter, relation: MonotoneFn, to: Parameter)

case class Parameter(
  name:               String,
  var min:            Value,
  private var _value: Value,
  var max:            Value) {

  private var bindingsFrom: List[Binding] = Nil
  private var bindingsTo: List[Binding] = Nil

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

    bindingsFrom.foreach(b => {
      val newValue = backtrackValue(v, b.relation, b.from.min, b.from.max)
      b.from.value(newValue)
    })

    _value = v

    bindingsTo.foreach(b => b.to._value = b.relation(v))
  }

  def functionOf(
    other:    Parameter,
    relation: MonotoneFn): Either[RelationError, Binding] = {

    _value = relation(other.value)

    val otherRange = Range(other.min, other.max)
    val range = Range(min, max)
    val mappedRange = otherRange.map(relation)
    val narrowedRange = mappedRange.intersection(range)

    if (narrowedRange.isEmpty)
      Left(NoRangeOverlap)
    else {

      val newMin = relation(other.min)
      val isNewMinTooSmall = newMin < min
      if (isNewMinTooSmall) {
        val otherNewMin = backtrackValue(min, relation, other.min, other.max)
        other.min = otherNewMin
      } else {
        min = newMin
      }

      val newMax = relation(other.max)
      val isNewMaxTooLarge = newMax > max
      if (isNewMaxTooLarge) {
        val otherNewMax = backtrackValue(max, relation, other.min, other.max)
        other.max = otherNewMax
      } else {
        max = newMax
      }

      val binding = Binding(other, relation, this)

      bindingsFrom = binding :: bindingsFrom
      other.bindingsTo = binding :: other.bindingsTo
      Right(binding)
    }
  }

  def inverseFunctionOf(other: Parameter, relation: MonotoneFn) = {

    min = relation(other.max)
    _value = relation(other.value)
    max = relation(other.min)

    bindingsFrom = Binding(other, relation, this) :: bindingsFrom
  }
}
