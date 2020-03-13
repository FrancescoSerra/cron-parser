package cron.parser

import RegExes._
import Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RegExesTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  val withinHundred: Int => Boolean = i => 0 <= i && i <= 99

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(500)

  "validEntry" should "match an entry or reject anything else" in forAll { i: Int =>
    if (withinHundred(i))
      validEntry.matches(i.toString) shouldBe true
    else
      validEntry.matches(i.toString) shouldBe false
  }

  "validAsterisk" should "match an asterisk with or without valid step or reject anything else" in forAll { i: Int =>
    if (withinHundred(i)) {
      validAsterisk.matches("*") shouldBe true
      validAsterisk.matches(s"*/$i") shouldBe true
    } else {
      validAsterisk.matches(s"$i") shouldBe false
      validAsterisk.matches(s"*/$i") shouldBe false
    }

  }

  "validRange" should "match a range with or without valid step or reject anything else" in forAll { range: Range =>
    val Range(start, finish, maybeStep) = range
    if (List(start, finish).forall(withinHundred) && maybeStep.forall(withinHundred))
      validRange.matches(range.toString) shouldBe true
    else
      validRange.matches(range.toString) shouldBe false
  }

  "validList" should "match a list of entries or reject anything else" in forAll(genNonEmptyListOfInt) { list: List[Int] =>
      if (list.forall(withinHundred))
        validList.matches(list.mkString(",")) shouldBe true
      else
        validList.matches(list.mkString(",")) shouldBe false
  }

  "validListOfRanges" should "match a list of ranges and optionally entries or reject anything else" in
    forAll(genNonEmptyListOfInt,genNonEmptyListOfRange) { (entries: List[Int], ranges: List[Range]) =>

    val entriesFirst = entries.zip(ranges).mkString(",")
    val rangesFirst = ranges.zip(entries).mkString(",")
    if (entries.forall(withinHundred) &&
      ranges.forall { case Range(a, b, c) => withinHundred(a) && withinHundred(b) && c.forall(withinHundred) }) {
      validListOfRanges.matches(entriesFirst) shouldBe true
      validListOfRanges.matches(rangesFirst) shouldBe true
      validListOfRanges.matches("0-4,8-12,5,6") shouldBe true
    } else {
      validListOfRanges.matches(entriesFirst) shouldBe false
      validListOfRanges.matches(rangesFirst) shouldBe false
    }
  }

  "validDay" should "match any possible day of the week or reject anything else" in
    forAll { days: List[Day] =>
      validDay.matches(days.mkString(",")) shouldBe true
    }

  "validMonth" should "match any month or reject anything else" in
    forAll { months: List[MonthLiteral] =>
      validMonth.matches(months.mkString(",")) shouldBe true
    }

}
