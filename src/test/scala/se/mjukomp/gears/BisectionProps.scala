package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

object BisectionProps extends Properties("Bisection") {

  property("backtrack min") = {
    val fn: (Double => Double) = x => Math.round((x + 0.5) / 2)
    val result = Bisection.backtrackMinValue(1.0, fn)

    all(
      "less or eq that 0.5" |: result <= 0.5,
      "greater than 0.49" |: result > 0.49,
      "equals 1" |: (fn(result) ?= 1.0),
      "is the smallest" |: (fn(result - Math.ulp(result)) ?= 0.0))
  }

  property("backtrack max") = {
    val fn: (Double => Double) = x => Math.round((x + 0.5) / 2)
    val result = Bisection.backtrackMaxValue(1.0, fn)

    all(
      "less that 2.5" |: result < 2.5,
      "greater than 2.49" |: result > 2.49,
      "equals 1" |: (fn(result) ?= 1.0),
      "is the largest" |: (fn(result + Math.ulp(result)) ?= 2.0))
  }

  property("infinity") = throws(classOf[RuntimeException]) {
    Bisection.backtrackMinValue(
      1, n => n, Range(Double.NegativeInfinity, 10))
  }

  property("not a number") = throws(classOf[RuntimeException]) {
    Bisection.backtrackMaxValue(
      1, n => n, Range(-22, Double.NaN))
  }

  property("backtracking addition") = forAll((
    source: Double,
    addition: Double) => {

    val fn = (x: Double) => x + addition
    val target = source + addition
    val backtrackedSource = Bisection.backtrack(target, fn)

    fn(backtrackedSource) ?= target
  })
}
