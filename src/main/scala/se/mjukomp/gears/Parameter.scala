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
    apply(name, value, defaultRange)
  def apply(name: String, value: Value, min: Value, max: Value): Parameter =
    apply(name, value, Range(min, max))
}

case class Parameter(
  name:               String,
  private var _value: Value,
  var range:          Range) {

  private var relationsFrom: List[Relation] = Nil
  private var relationsTo: List[Relation] = Nil

  def min() = range.min
  def max() = range.max
  def min(v: Value): Unit = range = range.copy(min = v)
  def max(v: Value): Unit = range = range.copy(max = v)
  def value() = _value

  @tailrec
  final def backtrackValue(target: Value, fn: MonotoneFn, range: Range): Double = {

    //System.err.println(s"[$rangeMin, $rangeMax]")
    if (range.min + math.ulp(range.min) >= range.max) {
      if (fn(range.min) < target) range.max else range.min
    } else {
      val middle = (range.min + range.max) / 2
      val middleIsLarge = fn(middle) > target

      val newRange =
        if (middleIsLarge) range.copy(max = middle)
        else range.copy(min = middle)

      backtrackValue(target, fn, newRange)
    }
  }

  def value(v: Value): Unit = {

    relationsFrom.foreach(b => {
      val newValue = backtrackValue(v, b.fn, b.from.range)
      b.from.value(newValue)
    })

    _value = v

    relationsTo.foreach(b => b.to._value = b.fn(v))
  }

  def functionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Relation] = {

    _value = fn(source.value)

    val narrowedRange = source.range
      .map(fn).intersection(range)

    if (narrowedRange.isEmpty)
      Left(NoRangeOverlap)
    else {

      val newMin = fn(source.min)
      val isNewMinTooSmall = newMin < min
      if (isNewMinTooSmall) {
        val otherNewMin = backtrackValue(min, fn, source.range)
        source.min(otherNewMin)
      } else {
        min(newMin)
      }

      val newMax = fn(source.max)
      val isNewMaxTooLarge = newMax > max
      if (isNewMaxTooLarge) {
        val otherNewMax = backtrackValue(max, fn, source.range)
        source.max(otherNewMax)
      } else {
        max(newMax)
      }

      val relation = Relation(source, fn, this)

      relationsFrom = relation :: relationsFrom
      source.relationsTo = relation :: source.relationsTo
      Right(relation)
    }
  }

  def inverseFunctionOf(source: Parameter, fn: MonotoneFn) = {

    min(fn(source.max))
    _value = fn(source.value)
    max(fn(source.min))

    relationsFrom = Relation(source, fn, this) :: relationsFrom
  }
}
