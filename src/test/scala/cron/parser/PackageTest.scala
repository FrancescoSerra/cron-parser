package cron.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PackageTest  extends AnyFlatSpec with Matchers {
  val listOfInts = List(1, 2, 3, 4, 5)
  val result = s"1 2 3 4 5"
  "override of toString in Field hierarchy classes" should "return the correct representation" in {
    Minute(listOfInts).toString shouldBe result
    Hour(listOfInts).toString shouldBe result
    DayOfMonth(listOfInts).toString shouldBe result
    Month(listOfInts).toString shouldBe result
    DayOfWeek(listOfInts).toString shouldBe result
    Command("foo").toString shouldBe "foo"
  }

  "override of toString in Error hierarchy classes" should "return the correct representation" in {
    ParsingError("boom").toString shouldBe "boom"
    InvalidFormat("boom").toString shouldBe "boom"
    IllegalValue("boom").toString shouldBe "boom"
  }
}