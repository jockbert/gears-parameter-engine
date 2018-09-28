package se.mjukomp.gears

case class Amount(var _value: Value, var listeners: List[ValueListener] = Nil) {

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
}
