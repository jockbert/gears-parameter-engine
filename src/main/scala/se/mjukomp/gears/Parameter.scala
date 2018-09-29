package se.mjukomp.gears

import scala.annotation.tailrec

/** A parameter with value and a valid range. */
object Parameter {
  private val valueMin: Value = Double.MinValue
  private val valueMax: Value = Double.MaxValue
  val defaultRange: se.mjukomp.gears.Range = Range(valueMin, valueMax)

  def apply(name: String, value: Value): Parameter =
    apply(name, Amount(value), defaultRange)

  def apply(name: String, value: Value, min: Value, max: Value): Parameter =
    apply(name, Amount(value), Range(min, max))
}

sealed trait ValueError
case object ValueNotInRange extends ValueError
case object OutsideMaxLimit extends ValueError
case object OutsideMinLimit extends ValueError

case class Parameter(
  name:               String,
  private val _value: Amount,
  staticRange:        Range) {

  private val _min = Amount(staticRange.min)
  private val _max = Amount(staticRange.max)

  def min: Amount = _min
  def max: Amount = _max
  def value: Amount = _value
  def range: Range = Range(_min.value, _max.value)
  def range(r: Range): Unit = {
    _min.value(r.min)
    _max.value(r.max)
  }

  def value(v: Value): Either[ValueError, Value] = {

    if (!range.includes(v))
      return Left(ValueNotInRange)

    _value.value(v)

    Right(_value.value)
  }
}
