package se.mjukomp.gears

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink
import org.scalacheck.Shrink.shrink

object Generators {

  val stringGen: Gen[String] = for {
    text: String <- Gen.asciiPrintableStr
  } yield text

  implicit def shrinkString: Shrink[String] =
    Shrink { ??? }
}
