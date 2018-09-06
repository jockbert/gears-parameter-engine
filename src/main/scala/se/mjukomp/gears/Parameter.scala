package se.mjukomp.gears

import Parameter._

object Parameter {
  type MonotoneFn = Double => Double

  type Value = Double

}

import scala.annotation.tailrec

case class Binding(from: Parameter, relation: MonotoneFn, to: Parameter)

case class Parameter(
  name:               String,
  var min:            Value,
  private var _value: Value,
  var max:            Value) {

  private var bindingsFrom: List[Binding] = Nil

  def value() = _value

  @tailrec
  final def backtrackValue(target: Value, relation: MonotoneFn, rangeMin: Value, rangeMax: Value): Double = {

    //System.err.println(s"[$rangeMin, $rangeMax]")
    if (rangeMin + math.ulp(rangeMin) >= rangeMax) {
      if (relation(rangeMin) < target) rangeMax else rangeMin
    } else {
      val rangeMiddle = (rangeMin + rangeMax) / 2
      val middleIsLarge = relation(rangeMiddle) > target

      val newMin = if (middleIsLarge) rangeMin else rangeMiddle
      val newMax = if (middleIsLarge) rangeMiddle else rangeMax

      backtrackValue(target, relation, newMin, newMax)
    }
  }

  def value(v: Value): Unit = {

    bindingsFrom.foreach(b => {
      val newValue = backtrackValue(v, b.relation, b.from.min, b.from.max)
      b.from.value(newValue)
    })

    _value = v

  }

  def functionOf(other: Parameter, relation: MonotoneFn) = {

    min = relation(other.min)
    _value = relation(other.value)
    max = relation(other.max)

    bindingsFrom = Binding(other, relation, this) :: bindingsFrom
  }

  def inverseFunctionOf(other: Parameter, relation: MonotoneFn) = {

    min = relation(other.max)
    _value = relation(other.value)
    max = relation(other.min)

    bindingsFrom = Binding(other, relation, this) :: bindingsFrom
  }
}
