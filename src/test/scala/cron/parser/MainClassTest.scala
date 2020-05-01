package cron.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.option._

class MainClassTest extends AnyFlatSpec with Matchers {
  val main = new MainClass

  "MainClass" should "store the first argument in optInput" in {
    main.main(Array("foo"))
    main.optInput shouldBe "foo".some
  }
}
