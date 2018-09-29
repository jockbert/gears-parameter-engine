package se.mjukomp.gears

import scala.annotation.tailrec

import BisectingRelation._

sealed trait RelationError
case object NoRangeOverlap extends RelationError

case class OperationalDomain(source: Range, target: Range) {
  def mergeDomains(fn: MonotoneFn) = {
    val narrowedTarget = source
      .map(fn).intersection(target)

    narrowedTarget match {
      case None => Left(NoRangeOverlap)
      case Some(newTarget) =>
        val newSource = Range(
          backtrackValue(newTarget.min, fn, source),
          backtrackValue(newTarget.max, fn, source))

        Right(OperationalDomain(newSource, newTarget))
    }
  }
}

case class RelationBuilder(target: Parameter) {

  def asFunctionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Unit] =
    OperationalDomain(source.staticRange, target.staticRange)
      .mergeDomains(fn)
      .map(domain => {

        target.value.set(fn(source.value.get()))

        // Update ranges
        target.range(domain.target)
        source.range(domain.source)

        // Create and register relation between values
        BisectingRelation(source.min, fn, domain, target.min)
        BisectingRelation(source.value, fn, domain, target.value)
        BisectingRelation(source.max, fn, domain, target.max)
        Right(())
      })

  def asInverseFunctionOf(
    source: Parameter,
    fn:     MonotoneFn): Either[RelationError, Unit] =
    OperationalDomain(source.staticRange, target.staticRange)
      .mergeDomains(fn)
      .map(domain => {

        target.value.set(fn(source.value.get()))

        // Update ranges
        target.range(domain.target)
        source.range(domain.source)

        // Create and register relation between values
        BisectingRelation(source.min, fn, domain, target.max)
        BisectingRelation(source.value, fn, domain, target.value)
        BisectingRelation(source.max, fn, domain, target.min)
        Right(())
      })

}

object Relation {

  def relate(target: Parameter): RelationBuilder =
    RelationBuilder(target)

}

/** The relation between two values, given a relation function. */
sealed trait Relation {
  def source: Amount
  def target: Amount
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
  source: Amount,
  fn:     MonotoneFn,
  domain: OperationalDomain,
  target: Amount)
  extends Relation {

  var blockFeedback: Boolean = false

  def withoutFeedback(changeFn: () => Unit): Unit =
    if (blockFeedback) ()
    else {
      blockFeedback = true
      changeFn()
      blockFeedback = false
    }

  val sourceValueListener: ValueListener =
    (value) => withoutFeedback(() => target.set(fn(value)))

  val targetValueListener: ValueListener =
    (value) => withoutFeedback(() => source.set(
      backtrackValue(target.get(), fn, domain.source)))

  source.addListener(sourceValueListener)
  target.addListener(targetValueListener)
}
