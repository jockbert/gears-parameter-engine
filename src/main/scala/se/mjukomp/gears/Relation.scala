package se.mjukomp.gears

import Parameter._

sealed trait RelationError
case object NoRangeOverlap extends RelationError

case class Relation(from: Parameter, fn: MonotoneFn, to: Parameter) {
  override def toString(): String = s"Relation(${from.name},${to.name})"
}
