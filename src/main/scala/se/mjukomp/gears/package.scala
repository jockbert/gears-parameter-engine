package se.mjukomp

package object gears {

  /** The type for a parameter value. */
  type Value = Double

  /**
   * A function over values.
   *
   * A strict monotone function has the
   * property that a function input can
   * be easily backtracked given an
   * output, using e.g. bisection method.
   *
   * The requirement of strict monotonicity
   * is relaxed to only monotonicity to
   * also be able to handle function with
   * plateaus (e.g. output in discrete
   * steps) such as <i>round</i> and
   * <i>floor</i>. This can then also
   * combined with rules on how to handle
   * plateaus, so an unique and consistent
   * function input value can be calculated
   * from function output.
   *
   *  @see https://en.wikipedia.org/wiki/Monotonic_function
   *  @see https://en.wikipedia.org/wiki/Bisection_method
   */
  type MonotoneFn = Value => Value

  type RangeListener = (Range) => Unit
  type ValueListener = (Value) => Unit
}
