package se.mjukomp.gears

sealed trait RelationError
case object NoRangeOverlap extends RelationError

/** The relation between two parameters, given a relation function. */
case class Relation(from: Parameter, fn: MonotoneFn, to: Parameter) {
  override def toString(): String = s"Relation(${from.name},${to.name})"
}
