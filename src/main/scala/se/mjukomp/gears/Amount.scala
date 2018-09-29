package se.mjukomp.gears

import java.util.Optional

case class Amount(private var _value: Value) {

  var listeners: List[ValueListener] = Nil
  var maxLimits: List[Limit] = Nil
  var minLimits: List[Limit] = Nil

  def get(): Value = _value
  def set(newValue: Value): Either[ValueError, Amount] =
    (newValue, minLimitObj(), maxLimitObj()) match {
      case (v, Some(limit), _) if v < limit.value() =>
        Left(OutsideMinLimit(v, limit.description, limit.value()))
      case (v, _, Some(limit)) if v > limit.value() =>
        Left(OutsideMaxLimit(v, limit.description, limit.value()))
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

  private def maxLimitObj(): Option[Limit] =
    maxLimits.sortWith(_.value() < _.value()).headOption

  def maxLimit(): Option[Value] =
    maxLimits.map(l => l.value()).sortWith(_ < _).headOption

  def addMinLimit(limit: Limit): Amount = {
    minLimits = limit :: minLimits
    this
  }

  private def minLimitObj(): Option[Limit] =
    minLimits.sortWith(_.value() > _.value()).headOption

  def minLimit(): Option[Value] =
    minLimits.map(l => l.value()).sortWith(_ > _).headOption
}
