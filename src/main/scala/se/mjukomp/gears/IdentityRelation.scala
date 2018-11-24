package se.mjukomp.gears

/** A one-to-one (identity function) relation using no backtracking. */
case class IdentityRelation(
  source: Amount,
  target: Amount) extends Relation with NoFeedback {

  source.addListener(value => withoutFeedback(() => target.set(value)))
  target.addListener(value => withoutFeedback(() => source.set(value)))
}
