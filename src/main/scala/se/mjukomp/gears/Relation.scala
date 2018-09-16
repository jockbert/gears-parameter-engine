package se.mjukomp.gears

import scala.annotation.tailrec

import BisectingRelation._

sealed trait RelationError
case object NoRangeOverlap extends RelationError

case class RelationBuilder(target: Parameter) {

  def asFunctionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Relation] = {

    val narrowedRange = source.range
      .map(fn).intersection(target.range)

    narrowedRange match {
      case None => Left(NoRangeOverlap)
      case Some(newRange) => {

        // Update target value
        target.value(fn(source.value))

        // Update ranges
        target.range(newRange)
        source.range(Range(
          backtrackValue(target.min, fn, source.range),
          backtrackValue(target.max, fn, source.range)))

        // Create and register relation to parameters
        Right(BisectingRelation(source, fn, target))
      }
    }
  }
}

object Relation {

  def relate(target: Parameter): RelationBuilder =
    RelationBuilder(target)

}

/** The relation between two parameters, given a relation function. */
sealed trait Relation {
  def from: Parameter
  def to: Parameter

  override def toString(): String =
    s"Relation(${from.name},${to.name})"
}

object BisectingRelation {

  @tailrec
  final def backtrackValue(
    target: Value,
    fn:     MonotoneFn,
    range:  Range): Value = {

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
}

case class BisectingRelation(
  override val from: Parameter,
  fn:                MonotoneFn,
  override val to:   Parameter)
  extends Relation {

  var blockFeedback: Boolean = false

  def withoutFeedback(changeFn: () => Unit): Unit =
    if (blockFeedback) ()
    else {
      blockFeedback = true
      changeFn()
      blockFeedback = false
    }

  val fromRangeListener: RangeListener =
    (range) => {}

  val fromValueListener: ValueListener =
    (value) => withoutFeedback(() => to.value(fn(value)))

  val toRangeListener: RangeListener =
    (range) => {}

  val toValueListener: ValueListener =
    (value) => withoutFeedback(() => from.value(
      backtrackValue(to.value, fn, from.range)))

  from.registerRangeListener(fromRangeListener)
  from.registerValueListener(fromValueListener)
  to.registerRangeListener(toRangeListener)
  to.registerValueListener(toValueListener)

}
