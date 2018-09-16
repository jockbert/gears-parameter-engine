package se.mjukomp.gears

import scala.annotation.tailrec

/** A parameter with value and a valid range. */
object Parameter {
  private val valueMin: Value = Double.MinValue
  private val valueMax: Value = Double.MaxValue
  val defaultRange = Range(valueMin, valueMax)

  def apply(name: String, value: Value): Parameter =
    apply(name, value, defaultRange)

  def apply(name: String, value: Value, min: Value, max: Value): Parameter =
    apply(name, value, Range(min, max))
}

sealed trait ValueError
case object ValueNotInRange extends ValueError

case class Parameter(
  name:               String,
  private var _value: Value,
  val staticRange:    Range) {

  private var _range = staticRange
  private var relationsFrom: List[Relation] = Nil
  private var relationsTo: List[Relation] = Nil

  def min() = range.min
  def max() = range.max
  def min(v: Value): Unit = _range = _range.copy(min = v)
  def max(v: Value): Unit = _range = _range.copy(max = v)
  def value() = _value
  def range() = _range

  @tailrec
  final def backtrackValue(target: Value, fn: MonotoneFn, range: Range): Double = {

    if (range.min + math.ulp(range.min) >= range.max) {
      if (fn(range.min) < target) range.max else range.min
    } else {
      val middle = (range.min + range.max) / 2
      val middleIsLarge = fn(middle) > target

      val newRange =
        if (middleIsLarge) range.copy(max = middle)
        else range.copy(min = middle)

      backtrackValue(target, fn, newRange)
    }
  }

  def value(v: Value): Either[ValueError, Value] = {

    if (!range.includes(v))
      return Left(ValueNotInRange)

    relationsFrom.foreach(b => {
      val newValue = backtrackValue(v, b.fn, b.from.range)
      b.from.value(newValue)
    })

    _value = v

    relationsTo.foreach(b => b.to._value = b.fn(v))

    Right(_value)
  }

  def functionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Relation] = {

    _value = fn(source.value)
    val narrowedRange = source.range
      .map(fn).intersection(range)

    narrowedRange match {
      case None => Left(NoRangeOverlap)
      case Some(newRange) => {
        this._range = newRange
        source._range = Range(
          backtrackValue(min, fn, source.range),
          backtrackValue(max, fn, source.range))

        val relation = Relation(source, fn, this)
        relationsFrom = relation :: relationsFrom
        source.relationsTo = relation :: source.relationsTo
        Right(relation)
      }
    }
  }

  def inverseFunctionOf(source: Parameter, fn: MonotoneFn) = {
    min(fn(source.max))
    _value = fn(source.value)
    max(fn(source.min))
    relationsFrom = Relation(source, fn, this) :: relationsFrom
  }
}
