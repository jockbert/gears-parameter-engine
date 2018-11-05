package se.mjukomp.gears

object BacktrackPreference {
  val MIN: BacktrackPreference = MinimumPreference
  val MAX: BacktrackPreference = MaximumPreference
  val MID: BacktrackPreference = MiddlePreference
  val LO_INT: BacktrackPreference = LowestIntPreference
  val HI_INT: BacktrackPreference = HightIntPreference

  type ValueFn = () => Value
}
import BacktrackPreference._

/**
 * Indicates preferred value in a range of possible values.
 *
 * Monotonic functions that for some reason has a plateau
 * ("mechanical play") in it's output range will have several
 * input values that leads to the same output value.
 * Backtracking that output value to a function input value
 * raises the question which function input value should be
 * preferred.
 */
trait BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn): Value
}

/** Minimum value in range preference. */
case object MinimumPreference extends BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn) = minFn()
}

/** Maximum value in range preference. */
case object MaximumPreference extends BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn) = maxFn()
}

/** Middle value in range preference. */
case object MiddlePreference extends BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn): Value =
    (minFn() / 2 + maxFn() / 2)
}

/** Lowest integer included in range or if not possible minimum value in range. */
case object LowestIntPreference extends BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn): Value = {
    val min = minFn()
    val lowestInt = Math.ceil(min)

    if (lowestInt < maxFn()) lowestInt
    else min
  }
}

/** Highest integer included in range or if not possible maximum value in range. */
case object HightIntPreference extends BacktrackPreference {
  def apply(minFn: ValueFn, maxFn: ValueFn): Value = {
    val max = maxFn()
    val highestInt = Math.floor(max)

    if (highestInt > minFn()) highestInt
    else max
  }
}
