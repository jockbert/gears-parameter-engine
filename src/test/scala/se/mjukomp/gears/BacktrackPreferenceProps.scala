package se.mjukomp.gears

import org.scalacheck.Prop._
import org.scalacheck.Properties

object BacktrackPreferenceProps extends Properties("BacktrackPreference") {

  // The decimals 625 is chosen for it is easy representable in floating point.
  val minValue = 1.625
  val maxValue = 43.625

  property("minimum") =
    verifyPreference(MinimumPreference, minValue, 1, 0)

  property("maximum") =
    verifyPreference(MaximumPreference, maxValue, 0, 1)

  property("middle") =
    verifyPreference(MiddlePreference, 22.625, 1, 1)

  property("lowest integer") =
    verifyPreference(LowestIntPreference, 2, 1, 1)

  property("highest integer") =
    verifyPreference(HightIntPreference, 43, 1, 1)

  def verifyPreference(
    preference:           BacktrackPreference,
    expectedResult:       Value,
    expectedCallCountMin: Int,
    expectedCallCountMax: Int) = {
    var callCountMax = 0
    var callCountMin = 0

    val result = preference.apply(
      () => { callCountMin = callCountMin + 1; minValue },
      () => { callCountMax = callCountMax + 1; maxValue })

    all(
      "min call count" |: (callCountMin ?= expectedCallCountMin),
      "max call count" |: (callCountMax ?= expectedCallCountMax),
      "preference" |: (result ?= expectedResult))
  }
}
