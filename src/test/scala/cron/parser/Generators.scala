package cron.parser

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import RegExes._

object Generators {
  val rangeGenerator: Gen[Range] = for {
    start <- choose(Int.MinValue, Int.MaxValue)
    end <- choose(Int.MinValue, Int.MaxValue)
    maybeStep <- option(choose(Int.MinValue, Int.MaxValue))
  } yield Range(start,end,maybeStep)

  implicit val arbitraryRange: Arbitrary[Range] = Arbitrary(rangeGenerator)

  val genNonEmptyListOfInt: Gen[List[Int]] = Gen.nonEmptyListOf[Int](Arbitrary.arbInt.arbitrary)
  val genNonEmptyListOfRange: Gen[List[Range]] = Gen.nonEmptyListOf[Range](rangeGenerator)

  val genDayString: Gen[String] = Gen.oneOf(listOfDays)
  val genMonthString: Gen[String] = Gen.oneOf(listOfMonths)

}
