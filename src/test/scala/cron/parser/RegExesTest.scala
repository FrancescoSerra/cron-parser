package cron.parser

import RegExes._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import Generators._

class RegExesTest extends FlatSpec with Matchers with PropertyChecks {
  val withinHundred : Int => Boolean = i => 0 <= i && i <= 99
  "validentry" should "match an entry or reject anything else" in forAll { i: Int =>
    if(withinHundred(i))
      validEntry.matches(i.toString) shouldBe true
    else
      validEntry.matches(i.toString) shouldBe false
  }

  "validAsterisk" should "match an asterisk with or without valid step or reject anything else" in forAll { i: Int =>
    if(withinHundred(i)) {
      validAsterisk.matches("*") shouldBe true
      validAsterisk.matches(s"*/$i") shouldBe true
    } else {
      validAsterisk.matches(s"$i") shouldBe false
      validAsterisk.matches(s"*/$i") shouldBe false
    }

  }

  "validRange" should "match a range with or without valid step or reject anything else" in forAll { range: Range =>
    val Range(start, finish, maybeStep) = range
    if(List(start, finish).forall(withinHundred) && maybeStep.forall(withinHundred))
      validRange.matches(range.toString) shouldBe true
    else
      validRange.matches(range.toString) shouldBe false
  }

  "validList" should "match a list of entries or reject anything else" in forAll { list: List[Int] =>
    whenever(list.nonEmpty) {
      if (list.forall(withinHundred))
        validList.matches(list.mkString(",")) shouldBe true
      else
        validList.matches(list.mkString(",")) shouldBe false
    }
  }

  "validListOfRanges" should "match a list of ranges and entries or reject anything else" in forAll { (entries: List[Int], ranges: List[Range]) =>
    whenever(entries.nonEmpty && ranges.nonEmpty) {

      val entriesFirst = entries.zip(ranges).mkString(",")
      val rangesFirst = ranges.zip(entries).mkString(",")

      println(entriesFirst)
      println(rangesFirst)
      if (entries.forall(withinHundred) &&
        ranges.forall { case Range(a,b,c) => withinHundred(a) && withinHundred(b) && c.forall(withinHundred) } ) {
        validListOfRanges.matches(entriesFirst) shouldBe true
        validListOfRanges.matches(rangesFirst) shouldBe true
      } else {
        validListOfRanges.matches(entriesFirst) shouldBe false
        validListOfRanges.matches(rangesFirst) shouldBe false
      }
    }
  }

}
