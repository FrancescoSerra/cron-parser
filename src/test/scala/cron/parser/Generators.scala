package cron.parser

import cats.syntax.option._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import RegExes._

import scala.util.Random

object Generators {
  val genEntry: Gen[Entry] = chooseNum(Int.MinValue,Int.MaxValue).map(Entry)
  val genAsterisk: Gen[Asterisk] = chooseNum(Int.MinValue,Int.MaxValue).map(i => Asterisk(i.some))
  val rangeGenerator: Gen[Range] = for {
    start <- chooseNum(Int.MinValue, Int.MaxValue)
    end <- chooseNum(Int.MinValue, Int.MaxValue)
    maybeStep <- option(chooseNum(Int.MinValue, Int.MaxValue))
  } yield Range(start,end,maybeStep)
  val genListOfRangeable: Gen[List[Rangeable]] = for {
    entry <- listOf[Entry](genEntry)
    range <- nonEmptyListOf[Range](rangeGenerator)
  } yield Random.shuffle(entry ::: range)

  val genNonEmptyListOfEntry: Gen[List[Entry]] = nonEmptyListOf[Entry](genEntry).map(_.distinct)
  val genNonEmptyListOfRange: Gen[List[Range]] = nonEmptyListOf[Range](rangeGenerator).map(_.distinct)

  val genDayString: Gen[Day] = oneOf(listOfDays)
  val genMonthString: Gen[MonthLiteral] = oneOf(listOfMonths)

  val genNonEmptyListOfDays: Gen[List[Day]] = nonEmptyListOf[Day](genDayString).map(_.distinct)
  val genNonEmptyListOfMonths: Gen[List[MonthLiteral]] = nonEmptyListOf[MonthLiteral](genMonthString).map(_.distinct)

  implicit val arbitraryEntry: Arbitrary[Entry] = Arbitrary(genEntry)
  implicit val arbitraryAsterisk: Arbitrary[Asterisk] = Arbitrary(genAsterisk)
  implicit val arbitraryRange: Arbitrary[Range] = Arbitrary(rangeGenerator)
  implicit val arbitraryListOfEntry: Arbitrary[List[Entry]] = Arbitrary(genNonEmptyListOfEntry)
  implicit val arbitraryListOfRange: Arbitrary[List[Range]] = Arbitrary(genNonEmptyListOfRange)
  implicit val arbitraryListOfRangeable: Arbitrary[List[Rangeable]] = Arbitrary(genListOfRangeable)
  implicit val arbitraryListOfDays: Arbitrary[List[Day]] = Arbitrary(genNonEmptyListOfDays)
  implicit val arbitraryListOfMonths: Arbitrary[List[MonthLiteral]] = Arbitrary(genNonEmptyListOfMonths)
}
