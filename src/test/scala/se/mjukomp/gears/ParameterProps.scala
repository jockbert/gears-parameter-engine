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

  property("DefaultRange") = {
    val p = Parameter("P", 6)

    all(
      "min" |: (p.min ?= Parameter.ValueMin),
      "value" |: (p.value ?= 6),
      "max" |: (p.max ?= Parameter.ValueMax))
  }

  property("BacktrackFunctionInverse") = {
    val aValue = 10;
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => 5 * x + 1

    b.functionOf(a, fn)
    b.value(fn(aValue))

    a.value ?= aValue
  }

  property("PropagateFunctionValue") = {
    val aValue = 10;
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => 5 * x + 1

    b.functionOf(a, fn)
    a.value(aValue)

    b.value ?= fn(aValue)
  }

  property("Align max restrict target B") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => x - 2

    b.functionOf(a, fn)

    all(
      "B max" |: (b.max ?= 998),
      "A max" |: (a.max ?= 1000))
  }

  property("Align max restrict source A") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => x + 2

    b.functionOf(a, fn)

    all(
      "B max" |: (b.max ?= 1000),
      "A max" |: (a.max ?= 998))
  }

  property("Align min restrict target B") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => x + 2

    b.functionOf(a, fn)

    all(
      "B min" |: (b.min ?= -998),
      "A min" |: (a.min ?= -1000))
  }

  property("Align min restrict source A") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => x - 2

    b.functionOf(a, fn)

    all(
      "B min" |: (b.min ?= -1000),
      "A min" |: (a.min ?= -998))
  }

  property("Detect range missmatch") = {
    val a = Parameter("A", -1000, 3, 1000)
    val b = Parameter("B", -1000, 4, 1000)
    val fn = (x: Double) => x + 3000

    b.functionOf(a, fn) ?= Left(NoRangeOverlap)
  }

  property("Detect single value range match") = {
    val a = Parameter("A", -1000, 3, 10)
    val b = Parameter("B", 10, 4, 1000)
    val fn = (x: Double) => x * 1

    b.functionOf(a, fn) ?= Right(Binding(a, fn, b))
  }

  def equals(
    actual: Parameter,
    min:    Value, value: Value, max: Value) =

    ((actual.min ?= min) :| (actual.name + " minimum")) &&
      ((actual.value ?= value) :| (actual.name + " value")) &&
      ((actual.max ?= max) :| (actual.name + " maximum"))
}
