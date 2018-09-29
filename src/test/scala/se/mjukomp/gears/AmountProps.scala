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
      .addMaxLimit(Limit(() => 5.0, "Foo"))
      .addMaxLimit(Limit(() => 7.7, "Bar"))
      .maxLimit() ?= Some(5.0)

  property("Has min limit") =
    Amount(3.3)
      .addMinLimit(Limit(() => 1.0, "Foo"))
      .addMinLimit(Limit(() => 2.7, "Bar"))
      .minLimit() ?= Some(2.7)

  property("Can not set outside of min limit") =
    Amount(3.3)
      .addMinLimit(Limit(() => 1.0, "Foo"))
      .set(0.9999) ?= Left(OutsideMinLimit(0.9999, "Foo", 1.0))

  property("Can not set outside of max limit") =
    Amount(3.3)
      .addMaxLimit(Limit(() => 4.0, "Baz"))
      .set(4.00001) ?= Left(OutsideMaxLimit(4.00001, "Baz", 4.0))
}
