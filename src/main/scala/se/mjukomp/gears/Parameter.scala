package se.mjukomp.gears

import scala.annotation.tailrec

/** A parameter with value and a valid range. */
object Parameter {
  private val valueMin: Value = Double.MinValue
  private val valueMax: Value = Double.MaxValue
  val defaultRange: se.mjukomp.gears.Range = Range(valueMin, valueMax)

  def apply(name: String, value: Value): Parameter =
    apply(name, value, defaultRange)

  def apply(name: String, value: Value, min: Value, max: Value): Parameter =
    apply(name, value, Range(min, max))
}

sealed trait ValueError
case object ValueNotInRange extends ValueError

case class Parameter(
  name:               String,
  private var _value: Value,
  staticRange:        Range) {

  private var _range = staticRange

  private var rangeListeners: List[RangeListener] = Nil
  private var valueListeners: List[ValueListener] = Nil

  def min: Value = range.min
  def max: Value = range.max
  def min(v: Value): Unit = _range = _range.copy(min = v)
  def max(v: Value): Unit = _range = _range.copy(max = v)
  def value: Value = _value
  def range: Range = _range
  def range(r: Range): Unit = _range = r

  def registerRangeListener(listener: RangeListener): Unit = {
    rangeListeners = listener :: rangeListeners
  }

  def registerValueListener(listener: ValueListener): Unit = {
    valueListeners = listener :: valueListeners
  }

  def value(v: Value): Either[ValueError, Value] = {

    if (!range.includes(v))
      return Left(ValueNotInRange)

    _value = v

    valueListeners.foreach(listener => listener(v))

    Right(_value)
  }
}
