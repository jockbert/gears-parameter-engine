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
}
