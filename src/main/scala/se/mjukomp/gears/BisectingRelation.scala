package se.mjukomp.gears

import Bisection.backtrack
import Bisection.backtrackMaxValue
import Bisection.backtrackMinValue

case class OperationalDomain(source: Range, target: Range) {

  /**
   * Calculates the valid function domain (source range),
   * given the unrelated source and target ranges, translation
   * function and backtrack preference.
   */
  def mergeDomains(
    fn:         MonotoneFn,
    preference: BacktrackPreference): Either[RelationError, OperationalDomain] = {
    val narrowedTarget = source
      .map(fn).intersection(target)

    narrowedTarget match {
      case None => Left(NoRangeOverlap)
      case Some(newTarget) =>
        val newSource = Range(
          backtrack(newTarget.min, fn, source, preference),
          backtrack(newTarget.max, fn, source, preference))

        Right(OperationalDomain(newSource, newTarget))
    }
  }
}

/** A relation that backtrack source value via bisection. */
case class BisectingRelation(
  source:     Amount,
  fn:         MonotoneFn,
  domain:     OperationalDomain,
  target:     Amount,
  preference: BacktrackPreference)
  extends Relation with NoFeedback {

  val sourceValueListener: ValueListener =
    value => withoutFeedback(() => target.set(fn(value)))

  val targetValueListener: ValueListener =
    value => withoutFeedback(() => {
      val preferedValue = preference.apply(
        () => backtrackMinValue(target.get(), fn, domain.source),
        () => backtrackMaxValue(target.get(), fn, domain.source))
      source.set(preferedValue)
    })

  source.addListener(sourceValueListener)
  target.addListener(targetValueListener)
}
