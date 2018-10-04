package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

object BacktrackingProps extends Properties("Backtracking") {

  property("backtracking") = {
    val fn = (x: Double) => x + 10.0
    BisectingRelation.backtrackValue(42, fn, Range(10, 50)) ?= 32.0
  }

  property("backtracking addition") = forAll((
    source: Double,
    addition: Double) => {

    val fn = (x: Double) => x + addition

    val target = source + addition

    val backtrackedSource = BisectingRelation.backtrackValue(target, fn, Range(Double.MinValue, Double.MaxValue))

    fn(backtrackedSource) ?= target

  })

}
