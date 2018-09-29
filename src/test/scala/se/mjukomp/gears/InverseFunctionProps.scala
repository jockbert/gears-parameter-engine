package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.gears.Relation._

object InverseFunctionProps extends Properties("InverseFunction") {

  property("Frequency inverse to period") = {
    val period = Parameter("period", 33, 0.0002, 20000)
    val frequency = Parameter("frequency", 0.5, 0.1, 4)

    relate(period).asInverseFunctionOf(frequency, f => 1 / f)

    equals(period, 2, 0.25, 10)
  }

  def equals(
    actual: Parameter,
    value:  Value,
    min:    Value,
    max:    Value) =
    (actual.name + " minimum" |: (actual.min.get() ?= min)) &&
      (actual.name + " value" |: (actual.value.get() ?= value)) &&
      (actual.name + " maximum" |: (actual.max.get() ?= max))

}
