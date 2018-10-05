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

    relate(b).asFunctionOf(a, fn) ?= Right(RelationBuilder(a))
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

  property("Revert to static range when removing relation") = {
    val a = Parameter("A", 3, -1000, 1000)
    val b = Parameter("B", 4, -1000, 1000)
    val fn = (x: Double) => x * 2.0

    val Right(relation) =
      relate(b).asFunctionOf(a, fn)

    relation.unregister()

    "Reverted dynamic range" |: (a.range ?= Range(-1000, 1000))
  }

  property("parameter series") = {
    val a = Parameter("A", 4, -100, 100)
    val b = Parameter("B", 4, -100, 100)
    val c = Parameter("C", 4, -100, 100)
    val d = Parameter("C", 4, -100, 100)
    val fn = (x: Double) => x + 10.0

    relate(c).asFunctionOf(b, fn)
    relate(d).asFunctionOf(c, fn)
    relate(b).asFunctionOf(a, fn)

    b.value.set(62.0)

    all(
      "A" |: equals(a, 52.0, -100, 70),
      "B" |: equals(b, 62.0, -90, 80),
      "C" |: equals(c, 72.0, -80, 90),
      "D" |: equals(d, 82.0, -70, 100))
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
