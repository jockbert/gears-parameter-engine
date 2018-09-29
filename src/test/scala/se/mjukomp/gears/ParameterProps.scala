package se.mjukomp.gears

import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.gears.Relation._

object ParameterProps extends Properties("Parameter") {

  property("Single Value") = {
    val single = Parameter("Single", 2, 0.1, 33)
    equals(single, 2, 0.1, 33)
  }

  /*
  property("Frequency and period example") = {
    val frequency = Parameter("frequency", 5, 1, 100)
    val period = Parameter("period", 33, 0.0002, 20000)

    equals(period, 33, 0.0002, 20000) :| "before" && {

      relate(period).asInverseFunctionOf(frequency, f => 1 / f)

      equals(period, 0.2, 0.01, 1) :| "after"
    }
  }*/

  property("Has default range") = {
    val p = Parameter("P", 6)

    all(
      "min" |: (p.min() ?= Parameter.defaultRange.min),
      "value" |: (p.value() ?= 6),
      "max" |: (p.max() ?= Parameter.defaultRange.max))
  }

  property("Backtrack function inverse") = {
    val aValue = 10;
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (x: Double) => 5 * x + 1

    relate(b).asFunctionOf(a, fn)

    b.value(fn(aValue))

    a.value() ?= aValue
  }

  property("Propagate function value") = {
    val aValue = 10;
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (a: Value) => 5 * a + 1

    relate(b).asFunctionOf(a, fn)

    a.value(aValue)

    b.value() ?= fn(aValue)
  }

  property("Align max restrict target B") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ - 2)

    all(
      "B max" |: (b.max() ?= 998),
      "A max" |: (a.max() ?= 1000))
  }

  property("Align max restrict source A") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ + 2)

    all(
      "B max" |: (b.max() ?= 1000),
      "A max" |: (a.max() ?= 998))
  }

  property("Align min restrict target B") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ + 2)

    all(
      "B min" |: (b.min() ?= -998),
      "A min" |: (a.min() ?= -1000))
  }

  property("Align min restrict source A") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ - 2)

    all(
      "B min" |: (b.min() ?= -1000),
      "A min" |: (a.min() ?= -998))
  }

  property("Detect range missmatch") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (x: Double) => x + 3000

    relate(b).asFunctionOf(a, fn) ?= Left(NoRangeOverlap)
  }

  property("Detect single value range match") = {
    // Will lead to a degenerate interval [10,10]
    val a = Parameter("A", 3, -1000, 10)
    val b = Parameter("B", 4, 10, 1000)
    val fn = (x: Double) => x * 1

    relate(b).asFunctionOf(a, fn) ?= Right(())
  }

  property("Detect value not in valid range") = {
    val a = Parameter("A", 3, -1000, 1000)

    all(
      "valid" |: (a.value(1000) ?= Right(Amount(1000))),
      "invalid" |: (a.value(1001) ?= Left(OutsideMaxLimit)))
  }

  property("Has static range") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (x: Double) => x * 2.0

    relate(b).asFunctionOf(a, fn)

    all(
      "Dynamic range" |: (a.range ?= Range(-500, 500)),
      "Static range" |: (a.staticRange ?= Range(-1000, 1000)))
  }

  def equals(
    actual: Parameter,
    value:  Value,
    min:    Value,
    max:    Value) =

    ((actual.min() ?= min) :| (actual.name + " minimum")) &&
      ((actual.value() ?= value) :| (actual.name + " value")) &&
      ((actual.max() ?= max) :| (actual.name + " maximum"))
}
