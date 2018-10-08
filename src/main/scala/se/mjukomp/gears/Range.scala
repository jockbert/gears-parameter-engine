package se.mjukomp.gears

object Range {
  def apply(a: Value, b: Value) =
    if (a < b) new Range(a, b) else new Range(b, a)

  val ALL = Range(Double.MinValue, Double.MaxValue)
}

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
