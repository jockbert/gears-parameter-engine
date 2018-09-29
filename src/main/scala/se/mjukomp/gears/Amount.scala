package se.mjukomp.gears

import java.util.Optional

case class Amount(private var _value: Value) {

  var listeners: List[ValueListener] = Nil
  var maxLimits: List[Limit] = Nil
  var minLimits: List[Limit] = Nil

  def apply(): Value = _value
  def value(): Value = _value
  def apply(newValue: Value): Either[ValueError, Amount] =
    value(newValue)
  def value(newValue: Value): Either[ValueError, Amount] =
    (newValue, minLimit(), maxLimit()) match {
      case (v, Some(min), _) if v < min => Left(OutsideMinLimit)
      case (v, _, Some(max)) if v > max => Left(OutsideMaxLimit)
      case (v, _, _) => {

        _value = newValue
        listeners.foreach(l => l(newValue))

        Right(this)
      }
    }

  def addListener(listener: ValueListener): Amount = {
    listeners = listener :: listeners
    this
  }

  def addMaxLimit(limit: Limit): Amount = {
    maxLimits = limit :: maxLimits
    this
  }

  def maxLimit(): Option[Value] =
    maxLimits.map(l => l()).sortWith(_ < _).headOption

  def addMinLimit(limit: Limit): Amount = {
    minLimits = limit :: minLimits
    this
  }

  def minLimit(): Option[Value] =
    minLimits.map(l => l()).sortWith(_ > _).headOption
}
