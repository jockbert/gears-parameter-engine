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

  _min.addMinLimit(() => staticRange.min)
  _min.addMaxLimit(() => _value.get())

  _value.addMaxLimit(() => _max.get())
  _value.addMinLimit(() => _min.get())

  _max.addMinLimit(() => _value.get())
  _max.addMaxLimit(() => staticRange.max)

  def min: Amount = _min
  def max: Amount = _max
  def value: Amount = _value
  def range: Range = Range(_min.get(), _max.get())
  def range(r: Range): Unit = {
    _min.set(r.min)
    _max.set(r.max)
  }
}
