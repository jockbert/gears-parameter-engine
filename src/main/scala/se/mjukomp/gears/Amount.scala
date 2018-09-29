package se.mjukomp.gears

import java.util.Optional

case class Amount(
  var _value:    Value,
  var listeners: List[ValueListener] = Nil,
  var maxLimits: List[Limit]         = Nil,
  var minLimits: List[Limit]         = Nil) {

  def apply(): Value = _value
  def value(): Value = _value
  def value(newValue: Value): Amount = {
    _value = newValue
    listeners.foreach(l => l(newValue))
    this
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
