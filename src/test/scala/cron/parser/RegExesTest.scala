package cron.parser

import RegExes._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class RegExesTest extends FlatSpec with Matchers with PropertyChecks {
  "validentry" should "match an entry or reject anything else" in forAll { i: Int =>
    if(i >= 0 && i <= 99)
      validEntry.matches(i.toString) shouldBe true
    else
      validEntry.matches(i.toString) shouldBe false
  }

  "validAsterisk" should "match an asterisk with or without valid step or reject anything else" in forAll { i: Int =>
    if(i >= 0 && i <= 99) {
      validAsterisk.matches("*") shouldBe true
      validAsterisk.matches(s"*/$i") shouldBe true
    } else {
      validAsterisk.matches(s"$i") shouldBe false
      validAsterisk.matches(s"*/$i") shouldBe false
    }

  }

  "validRange" should "match a range with or without valid step or reject anything else" in forAll { (start: Int, finish: Int, maybeStep: Int) =>
    if(List(start, finish, maybeStep).forall(i => i >= 0 && i <= 99)) {
        validRange.matches(s"$start-$finish") shouldBe true
        validRange.matches(s"$start-$finish/$maybeStep") shouldBe true
    } else {
      validRange.matches(s"$start-$finish") shouldBe false
      validRange.matches(s"$start-$finish/$maybeStep") shouldBe false
    }
  }

  "validList" should "match a list of entries or reject anything else" in forAll {
    
  }

}
