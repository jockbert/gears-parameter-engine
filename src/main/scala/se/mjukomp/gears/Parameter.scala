package se.mjukomp.gears

object Parameter {
  type MonotoneRelation = Double => Double

  type Value = Double

}

import Parameter._

case class BindingTo(target: Parameter, relation: MonotoneRelation)

case class Parameter(
  name:      String,
  var min:   Value,
  var value: Value,
  var max:   Value) {

  private var bindings: List[BindingTo] = Nil

  def functionOf(other: Parameter, relation: MonotoneRelation) = {

    min = relation(other.min)
    value = relation(other.value)
    max = relation(other.max)

    bindings = BindingTo(other, relation) :: bindings
  }

  def inverseFunctionOf(other: Parameter, relation: MonotoneRelation) = {

    min = relation(other.max)
    value = relation(other.value)
    max = relation(other.min)

    bindings = BindingTo(other, relation) :: bindings
  }
}
