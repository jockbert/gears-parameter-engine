package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.gears.Parameter._
import org.scalacheck.Prop
import scala.collection.immutable.Stream

object ParameterProps extends Properties("Parameter") {

  property("Single Value") = {
    val single = Parameter("Single", 0.1, 2, 33)
    equals(single, 0.1, 2, 33)
  }

  property("FrequencyAndPeriod") = {
    val frequency = Parameter("frequency", 1, 5, 100)
    val period = Parameter("period", 0.0002, 33, 20000)

    equals(period, 0.0002, 33, 20000) :| "before" && {

      period.inverseFunctionOf(frequency, f => 1 / f)

      equals(period, 0.01, 0.2, 1) :| "after"
    }
  }

  property("BacktrackFunctionInverseValue") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 3, 1000)
    val fn = (x: Double) => 5 * x + 1

    b.functionOf(a, fn)

    val aValue = 10;
    val bValue = fn(aValue)

    b.value(bValue)

    a.value ?= aValue
  }

  def equals(
    actual: Parameter,
    min:    Value, value: Value, max: Value) =

    ((actual.min ?= min) :| (actual.name + " minimum")) &&
      ((actual.value ?= value) :| (actual.name + " value")) &&
      ((actual.max ?= max) :| (actual.name + " maximum"))
}
