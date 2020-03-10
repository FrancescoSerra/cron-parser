package cron.parser

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._

object Generators {
  val rangeGenerator: Gen[Range] = for {
    start <- choose(Int.MinValue, Int.MaxValue)
    end <- choose(Int.MinValue, Int.MaxValue)
    maybeStep <- option(choose(Int.MinValue, Int.MaxValue))
  } yield Range(start,end,maybeStep)

  implicit val arbitraryRange: Arbitrary[Range] = Arbitrary(rangeGenerator)
}
