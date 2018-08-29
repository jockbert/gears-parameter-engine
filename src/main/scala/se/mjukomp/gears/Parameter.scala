package se.mjukomp.gears

object Parameter {
  type MonotoneRelation = Double => Double

  type Value = Double

}

import Parameter._

case class BindingTo(target: Parameter, relation: MonotoneRelation)

case class Parameter(name: String, var min: Value, var max: Value) {

  private var bindings: List[BindingTo] = Nil

  var value: Value = min

  def apply = value
  def update(newValue: Value) = { value = newValue }

  def interlock(other: Parameter, relation: MonotoneRelation) = {

    other.min = relation(min)
    other.value = relation(value)
    other.max = relation(max)

    bindings = BindingTo(other, relation) :: bindings
  }

  def inversInterlock(other: Parameter, relation: MonotoneRelation) = {

    other.max = relation(min)
    other.value = relation(value)
    other.min = relation(max)

    bindings = BindingTo(other, relation) :: bindings
  }
}
