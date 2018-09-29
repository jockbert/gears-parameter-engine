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
case class OutsideMaxLimit(
  violator:      Value,
  description:   String,
  violatedLimit: Value) extends ValueError

case class OutsideMinLimit(
  violator:      Value,
  description:   String,
  violatedLimit: Value) extends ValueError

case class Parameter(
  name:               String,
  private val _value: Amount,
  staticRange:        Range) {

  private val _min = Amount(staticRange.min)
  private val _max = Amount(staticRange.max)

  _min.addMinLimit(Limit(() => staticRange.min, s"$name static min"))
  _min.addMaxLimit(Limit(() => _value.get(), s"$name value"))

  _value.addMaxLimit(Limit(() => _max.get(), s"$name max"))
  _value.addMinLimit(Limit(() => _min.get(), s"$name min"))

  _max.addMinLimit(Limit(() => _value.get(), s"$name value"))
  _max.addMaxLimit(Limit(() => staticRange.max, s"$name static max"))

  def min: Amount = _min
  def max: Amount = _max
  def value: Amount = _value
  def range: Range = Range(_min.get(), _max.get())
  def range(r: Range): Unit = {
    _min.set(r.min)
    _max.set(r.max)
  }
}
