package se.mjukomp.gears

import scala.annotation.tailrec

import Bisection._
import BacktrackPreference._

sealed trait RelationError
case object NoRangeOverlap extends RelationError

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
trait Relation {
  def source: Amount
  def target: Amount
}
