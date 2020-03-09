package cron.parser

import RegExes._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class RegExesTest extends FlatSpec with Matchers with PropertyChecks {
  "validentry" should "match one or two digits" in forAll { i: Int =>
    whenever(i >= 0 && i <= 99) {
      validEntry.matches(i.toString) shouldBe true
    }

  }

  it should "fail with an invalid string" in forAll { i: Int =>
    whenever(i < 0 || i > 99) {
      validEntry.matches(i.toString) shouldBe false
    }
  }

  "validAsterisk" should "match an asterisk with or without valid step" in forAll("step") { i: Int =>
    whenever(i >= 0 && i <= 99) {
      validAsterisk.matches("*") shouldBe true
      validAsterisk.matches(s"*/$i") shouldBe true
    }
  }

  it should "fail with an invalid string" in forAll { i: Int =>
    whenever(i < 0 || i > 99) {
      validAsterisk.matches(s"$i") shouldBe false
      validAsterisk.matches(s"*/$i") shouldBe false
    }
  }

  "validRange" should "match a range with or without valid step" in forAll("start","finish","step") { (start: Int, finish: Int, maybeStep: Int) =>
    println(s"start: $start - finish: $finish - step: $maybeStep")
    whenever(
      (start >= 0 && start <= 99) &&
      (finish >= 0 && finish <= 99) &&
      (maybeStep >= 0 && maybeStep <= 99)) {
        validRange.matches(s"$start-$finish") shouldBe true
        validRange.matches(s"$start-$finish/$maybeStep") shouldBe true
    }
  }

  it should "fail with an invalid string" in forAll { (start: Int, finish: Int, maybeStep: Option[Int]) =>
    whenever(
      (start < 0 || start > 99) ||
        (finish < 0 || finish > 99) ||
        (maybeStep.isEmpty || (maybeStep.isDefined && maybeStep.get < 0 || maybeStep.get > 99))) {
      if (maybeStep.isEmpty)
        validRange.matches(s"$start-$finish") shouldBe false
      else
        validRange.matches(s"$start-$finish/${maybeStep.get}") shouldBe false
    }
  }
}
