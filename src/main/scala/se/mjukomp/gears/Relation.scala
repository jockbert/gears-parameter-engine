package se.mjukomp.gears

import scala.annotation.tailrec

import BisectingRelation._
import BacktrackPreference._

sealed trait RelationError
case object NoRangeOverlap extends RelationError

case class OperationalDomain(source: Range, target: Range) {
  def mergeDomains(
    fn:         MonotoneFn,
    preference: BacktrackPreference) = {
    val narrowedTarget = source
      .map(fn).intersection(target)

    narrowedTarget match {
      case None => Left(NoRangeOverlap)
      case Some(newTarget) =>
        val newSource = Range(
          backtrackValue(newTarget.min, fn, source, preference),
          backtrackValue(newTarget.max, fn, source, preference))

        Right(OperationalDomain(newSource, newTarget))
    }
  }
}

case class RelationBuilder(target: Parameter) {

  def asFunctionOf(
    source:     Parameter,
    fn:         MonotoneFn,
    preference: BacktrackPreference = MIN): Either[RelationError, RelationBuilder] =
    OperationalDomain(source.range, target.range)
      .mergeDomains(fn, preference)
      .map(domain => {

        target.value.set(fn(source.value.get()))

        // Update ranges
        target.range(domain.target)
        source.range(domain.source)

        // Create and register relation between values
        BisectingRelation(source.min, fn, domain, target.min, preference)
        BisectingRelation(source.value, fn, domain, target.value, preference)
        BisectingRelation(source.max, fn, domain, target.max, preference)
        RelationBuilder(source)
      })

  def asInverseFunctionOf(
    source:     Parameter,
    fn:         MonotoneFn,
    preference: BacktrackPreference = MIN): Either[RelationError, Unit] =
    OperationalDomain(source.staticRange, target.staticRange)
      .mergeDomains(fn, preference)
      .map(domain => {

        target.value.set(fn(source.value.get()))

        // Update ranges
        target.range(domain.target)
        source.range(domain.source)

        // Create and register relation between values
        BisectingRelation(source.min, fn, domain, target.max, preference)
        BisectingRelation(source.value, fn, domain, target.value, preference)
        BisectingRelation(source.max, fn, domain, target.min, preference)
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

  def backtrackValue(
    target:     Value,
    fn:         MonotoneFn,
    range:      Range               = Range.ALL,
    preference: BacktrackPreference = MinimumPreference) =
    preference.apply(
      () => backtrackMinValue(target, fn, range),
      () => backtrackMaxValue(target, fn, range))

  def backtrackMinValue(
    target:     Value,
    fn:         MonotoneFn,
    inputRange: Range      = Range.ALL): Value =
    backtrack(target, fn, inputRange, target <= _)

  def backtrackMaxValue(
    target:     Value,
    fn:         MonotoneFn,
    inputRange: Range      = Range.ALL): Value =
    backtrack(target, fn, inputRange, target < _)

  private def backtrack(
    target:     Value,
    fn:         MonotoneFn,
    inputRange: Range              = Range.ALL,
    goLow:      (Value) => Boolean): Value = {

    @tailrec
    def search(range: Range): Value =
      if (range.min + math.ulp(range.min) >= range.max) {
        if (fn(range.min) < target) range.max else range.min
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

case class BisectingRelation(
  source:     Amount,
  fn:         MonotoneFn,
  domain:     OperationalDomain,
  target:     Amount,
  preference: BacktrackPreference)
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
    (value) => withoutFeedback(() => {
      val preferedValue = preference.apply(
        () => backtrackMinValue(target.get(), fn, domain.source),
        () => backtrackMaxValue(target.get(), fn, domain.source))
      source.set(preferedValue)
    })

  source.addListener(sourceValueListener)
  target.addListener(targetValueListener)
}
