package cron.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.option._
import cats.syntax.either._
import cats.data.{NonEmptyChain, NonEmptyList}
import cats.effect.{ExitCode, IO}

class MainClassTest extends AnyFlatSpec with Matchers {
  val main = new MainClass

  "MainClass.main" should "return an error string if no argument is passed to it" in {
    main.translate(None) shouldBe "Please provide a cron line to interpret".asLeft
  }

  it should "return a valid list of tuples if a valid argument is passed in" in {
    main.translate("25 6 * * * root /usr/sbin/anacron".some) shouldBe
      NonEmptyList.of(
        ("minute", Minute(List(25))),
        ("hour", Hour(List(6))),
        ("day of month", DayOfMonth((1 to 31).toList)),
        ("month", Month((1 to 12).toList)),
        ("day of week", DayOfWeek((0 to 7).toList)),
        ("command", Command("root /usr/sbin/anacron"))
      ).asRight
  }

  it should "return an error if an invalid argument is passed in" in {
    main.translate("invalid".some) shouldBe
      "input couldn't be parsed into the expected tokens: invalid".asLeft
  }

  "MainClass.printRes" should "print the right output if the input is a Right" in {
    main.printRes(Right(NonEmptyList.one(("minute", Minute(List(1)))))).unsafeRunSync shouldBe
      SuccessResult
  }

  it should "print a failure if the input is Left" in {
    main.printRes(Left("gahh")).unsafeRunSync shouldBe
      ErrorResult
  }

  "MainClass.run" should "exit with success if correct input was passed" in {
    main.run(List("25 6 * * * root /usr/sbin/anacron")).unsafeRunSync shouldBe
      ExitCode.Success
  }

  it should "exit with failure if invalid input was passed" in {
    main.run(List("bogus")).unsafeRunSync shouldBe
      ExitCode.Error
  }
}
