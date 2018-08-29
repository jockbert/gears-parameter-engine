package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

import se.mjukomp.gears.Parameter._
import org.scalacheck.Prop
import scala.collection.immutable.Stream

object ParameterProps extends Properties("Parameter") {

  property("Single Value") = {
    val single = Parameter("Single", 0.1, 33)

    (single.value ?= 0.1) &&
      (single.min ?= 0.1) &&
      (single.max ?= 33)
  }

  property("Frequency and period") = {

    val frequency = Parameter("frequency", 1, 100)
    val period = Parameter("period", 0.0002, 20000)

    val minBefore = (period.min ?= 0.0002) :|
      "Period.min not interlocked to frequency"

    val valueBefore = (period.value ?= 0.0002) :|
      "Period.value not interlocked to frequency"

    val maxBefore = (period.max ?= 20000) :|
      "Period.max not interlocked to frequency"

    frequency.inversInterlock(period, f => 1 / f)

    val minAfter = (period.min ?= 0.01) :|
      "Period.min interlocked to frequency"

    val valueAfter = (period.value ?= 1) :|
      "Period.value interlocked to frequency"

    val maxAfter = (period.max ?= 1) :|
      "Period.max interlocked to frequency"

    minBefore && valueBefore && maxBefore && minAfter && valueAfter && maxAfter
  }
}
