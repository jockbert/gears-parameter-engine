package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

object AmountProps extends Properties("Amount") {

  property("Has a value") =
    Amount(4.3).value ?= 4.3

  property("Can register listener") = {
    var notified: Value = 0.0

    val a: Amount = Amount(3.3).
      addListener(notified = _)
      .value(55.5)

    notified ?= 55.5
  }

  property("Has max limit") =
    Amount(3.3)
      .addMaxLimit(() => 5.0)
      .addMaxLimit(() => 7.7)
      .maxLimit() ?= Some(5.0)

  property("Has min limit") =
    Amount(3.3)
      .addMinLimit(() => 1.0)
      .addMinLimit(() => 2.7)
      .minLimit() ?= Some(2.7)

}
