package se.mjukomp.gears

import scala.annotation.tailrec

/** Tools for backtrack functions using bisection. */
object Bisection {

  def backtrack(
    output:     Value,
    fn:         MonotoneFn,
    inputRange: Range               = Range.ALL,
    preference: BacktrackPreference = MinimumPreference) =
    preference.apply(
      () => backtrackMinValue(output, fn, inputRange),
      () => backtrackMaxValue(output, fn, inputRange))

  def backtrackMinValue(
    output:     Value,
    fn:         MonotoneFn,
    inputRange: Range      = Range.ALL): Value =
    directedBacktrack(output, fn, inputRange, output <= _)

  def backtrackMaxValue(
    output:     Value,
    fn:         MonotoneFn,
    inputRange: Range      = Range.ALL): Value =
    directedBacktrack(output, fn, inputRange, output < _)

  private def directedBacktrack(
    output:     Value,
    fn:         MonotoneFn,
    inputRange: Range              = Range.ALL,
    goLow:      (Value) => Boolean): Value = {

    @tailrec
    def search(range: Range): Value =
      if (range.min + math.ulp(range.min) >= range.max) {
        if (fn(range.min) < output) range.max else range.min
      } else {
        val middle = range.min / 2 + range.max / 2
        val newRange =
          if (goLow(fn(middle))) Range(range.min, middle)
          else Range(middle, range.max)
        search(newRange)
      }

    if (isSearchable(inputRange)) search(inputRange)
    else throw new RuntimeException(s"Bad backtrack range $inputRange.")
  }

  private def isSearchable(range: Range): Boolean =
    isSearchable(range.min) && isSearchable(range.max)

  private def isSearchable(value: Value): Boolean =
    !value.isInfinite() && !value.isNaN()
}
