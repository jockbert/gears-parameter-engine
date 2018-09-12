package se.mjukomp.gears

/** A value range (i.e. closed interval). */
case class Range(min: Value, max: Value) {

  def intersection(other: Range) =
    if (other.max < min || other.min > max) None
    else Some(Range(
      Math.max(min, other.min),
      Math.min(max, other.max)))

  def map(fn: MonotoneFn) =
    Range(fn(min), fn(max))

  def includes(v: Value) =
    min <= v && v <= max
}
