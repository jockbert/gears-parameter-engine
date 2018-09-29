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

  property("Has default range") = {
    val p = Parameter("P", 6)

    all(
      "min" |: (p.min.get() ?= Parameter.defaultRange.min),
      "value" |: (p.value.get() ?= 6),
      "max" |: (p.max.get() ?= Parameter.defaultRange.max))
  }

  property("Backtrack function inverse") = {
    val aValue = 10;
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (x: Double) => 5 * x + 1

    relate(b).asFunctionOf(a, fn)

    b.value.set(fn(aValue))

    a.value.get() ?= aValue
  }

  property("Propagate function value") = {
    val aValue = 10;
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (a: Value) => 5 * a + 1

    relate(b).asFunctionOf(a, fn)

    a.value.set(aValue)

    b.value.get() ?= fn(aValue)
  }

  property("Align max restrict target B") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ - 2)

    all(
      "B max" |: (b.max.get() ?= 998),
      "A max" |: (a.max.get() ?= 1000))
  }

  property("Align max restrict source A") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ + 2)

    all(
      "B max" |: (b.max.get() ?= 1000),
      "A max" |: (a.max.get() ?= 998))
  }

  property("Align min restrict target B") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ + 2)

    all(
      "B min" |: (b.min.get() ?= -998),
      "A min" |: (a.min.get() ?= -1000))
  }

  property("Align min restrict source A") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)

    relate(b).asFunctionOf(a, _ - 2)

    all(
      "B min" |: (b.min.get() ?= -1000),
      "A min" |: (a.min.get() ?= -998))
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
      "valid" |: (a.value.set(1000) ?= Right(Amount(1000))),
      "invalid" |: (a.value.set(1001) ?= Left(OutsideMaxLimit(1001, "A max", 1000))))
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
    (actual.name + " minimum" |: (actual.min.get() ?= min)) &&
      (actual.name + " value" |: (actual.value.get() ?= value)) &&
      (actual.name + " maximum" |: (actual.max.get() ?= max))
}
