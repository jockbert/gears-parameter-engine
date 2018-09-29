package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

object AmountProps extends Properties("Amount") {

  property("Has a value") =
    Amount(4.3).get() ?= 4.3

  property("Can register listener") = {
    var notified: Value = 0.0

    Amount(3.3)
      .addListener(notified = _)
      .set(55.5)

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

  property("Can not set outside of min limit") =
    Amount(3.3)
      .addMinLimit(() => 1.0)
      .set(0.9999) ?= Left(OutsideMinLimit)

  property("Can not set outside of max limit") =
    Amount(3.3)
      .addMaxLimit(() => 4.0)
      .set(4.00001) ?= Left(OutsideMaxLimit)

}
