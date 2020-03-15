package cron.parser

import RegExes._
import Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RegExesTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  val withinHundred: Int => Boolean = i => 0 <= i && i <= 99

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(500)

  "validEntry" should "match an entry or reject anything else" in forAll { i: Entry =>
    if (withinHundred(i.value))
      validEntry.matches(i.toString) shouldBe true
    else
      validEntry.matches(i.toString) shouldBe false
  }

  "validAsterisk" should "match an asterisk with or without valid step or reject anything else" in forAll { a: Asterisk =>
    if (a.maybeStep.forall(withinHundred) || a.maybeStep.isEmpty) validAsterisk.matches(a.toString) shouldBe true
    else validAsterisk.matches(a.toString) shouldBe false
  }

  "validRange" should "match a range with or without valid step or reject anything else" in forAll { range: Range =>
    val Range(start, finish, maybeStep) = range
    if (List(start, finish).forall(withinHundred) && maybeStep.forall(withinHundred))
      validRange.matches(range.toString) shouldBe true
    else
      validRange.matches(range.toString) shouldBe false
  }

  "validList" should "match a list of entries or reject anything else" in forAll { list: List[Entry] =>
      if (list.map(_.value).forall(withinHundred))
        validList.matches(list.mkString(",")) shouldBe true
      else
        validList.matches(list.mkString(",")) shouldBe false
  }

  "validListOfRanges" should "match a list of ranges and optionally entries or reject anything else" in
    forAll(genNonEmptyListOfEntry,genNonEmptyListOfRange) { (entries: List[Entry], ranges: List[Range]) =>

    val entriesFirst = entries.zip(ranges).mkString(",")
    val rangesFirst = ranges.zip(entries).mkString(",")

    if (ranges.forall { case Range(a, b, c) => withinHundred(a) && withinHundred(b) && c.forall(withinHundred) }) {
      validListOfRanges.matches(ranges.mkString(",")) shouldBe true
      if (entries.map(_.value).forall(withinHundred)) {
        validListOfRanges.matches(entriesFirst) shouldBe true
        validListOfRanges.matches(rangesFirst) shouldBe true
      } else {
        validListOfRanges.matches(entriesFirst) shouldBe false
        validListOfRanges.matches(rangesFirst) shouldBe false
      }
    } else {
      validListOfRanges.matches(ranges.mkString(",")) shouldBe false
      validListOfRanges.matches(entriesFirst) shouldBe false
      validListOfRanges.matches(rangesFirst) shouldBe false
    }
  }

  "validDay" should "match any possible day of the week or reject anything else" in forAll { days: List[Day] =>
      validDay.matches(days.mkString(",")) shouldBe true
    }

  "validMonth" should "match any month or reject anything else" in forAll { months: List[MonthLiteral] =>
      validMonth.matches(months.mkString(",")) shouldBe true
    }

}
