package cron.parser

import org.scalatest.{FlatSpec, Matchers}
import Functions._
import cats.data.Chain
import cats.data.Validated.Valid
import cats.syntax.validated._

class FunctionsTest extends FlatSpec with Matchers {
  "parseLine" should "return the tokenised parts of a cron line" in {
    val line = "*/15 0 1,15 * 1-5 /usr/bin/find"

    parseLine(line) shouldBe Right(LineTokens(
            maybeMinute = "*/15",
            maybeHour = "0",
            maybeDayOfMonth = "1,15",
            maybeMonth = "*",
            maybeDayOfWeek = "1-5",
            maybeCommand = "/usr/bin/find"
          )
    )
  }

  it should "return an error if the line can't be tokenised in the expected number of elements" in {
    val line = "foo bar bla"

    parseLine(line) shouldBe Left(ParsingError("input couldn't be parsed into the expected tokens: foo bar bla"))
  }

  "composeToMonth" should "return a valid list of months if the field contains legal values" in {
    val entry = "1"
    val asterisk = "*"
    val range = "8-11"
    val listOfEntries = "1,2,5,9,11"
    val listOfRanges = "1-4,8-12"
    val rangeWithStep = "1-12/2"
    val asteriskWithStep = "*/2"
    val month = "nov"

    composeToMonth(entry) shouldBe Valid(Month(List(1)))
    composeToMonth(asterisk) shouldBe Valid(Month((1 to 12).toList))
    composeToMonth(range) shouldBe Valid(Month((8 to 11).toList))
    composeToMonth(listOfEntries) shouldBe Valid(Month(List(1,2,5,9,11)))
    composeToMonth(listOfRanges) shouldBe Valid(Month((1 to 4).toList ++ (8 to 12).toList))
    composeToMonth(rangeWithStep) shouldBe Valid(Month(List(2,4,6,8,10,12)))
    composeToMonth(asteriskWithStep) shouldBe Valid(Month((1 to 12).toList.filter(_ % 2 == 0)))
    composeToMonth(month) shouldBe Valid(Month(List(11)))
  }

  it should "return a failure if the month field contains illegal values" in {
    val entry = "60"
    val range = "8-80"
    val listOfEntries = "61,62"
    val listOfRanges = "0-4,0-99/2"
    val day = "Tue"

    composeToMonth(entry) shouldBe IllegalValue("60 is not a valid month value").invalidNec
    composeToMonth(range) shouldBe IllegalValue("8-80 is not a valid month value").invalidNec
    composeToMonth(listOfEntries) shouldBe IllegalValue("61,62 is not a valid month value").invalidNec
    composeToMonth(listOfRanges) shouldBe IllegalValue("0-4,0-99/2 is not a valid month value").invalidNec
    composeToMonth(day) shouldBe IllegalValue("Tue is not a valid month value").invalidNec
  }

  "composeToDayOfWeek" should "return a valid list of days of week if the field contains legal values" in {
    val entry = "1"
    val asterisk = "*"
    val range = "3-7"
    val listOfEntries = "1,2,5"
    val listOfRanges = "1-4,4-7"
    val rangeWithStep = "1-6/2"
    val asteriskWithStep = "*/2"
    val day = "tue"

    composeToDayOfWeek(entry) shouldBe Valid(DayOfWeek(List(1)))
    composeToDayOfWeek(asterisk) shouldBe Valid(DayOfWeek((0 to 7).toList))
    composeToDayOfWeek(range) shouldBe Valid(DayOfWeek((3 to 7).toList))
    composeToDayOfWeek(listOfEntries) shouldBe Valid(DayOfWeek(List(1,2,5)))
    composeToDayOfWeek(listOfRanges) shouldBe Valid(DayOfWeek(((1 to 4).toList ++ (4 to 7).toList).toSet.toList.sorted))
    composeToDayOfWeek(rangeWithStep) shouldBe Valid(DayOfWeek(List(2,4,6)))
    composeToDayOfWeek(asteriskWithStep) shouldBe Valid(DayOfWeek((0 to 7).toList.filter(_ % 2 == 0)))
    composeToDayOfWeek(day) shouldBe Valid(DayOfWeek(List(2)))
  }

  it should "return a failure if the month field contains illegal values" in {
    val entry = "60"
    val range = "8-80"
    val listOfEntries = "61,62"
    val listOfRanges = "0-4,0-99/2"
    val month = "Nov"

    composeToDayOfWeek(entry) shouldBe IllegalValue("60 is not a valid day of week value").invalidNec
    composeToDayOfWeek(range) shouldBe IllegalValue("8-80 is not a valid day of week value").invalidNec
    composeToDayOfWeek(listOfEntries) shouldBe IllegalValue("61,62 is not a valid day of week value").invalidNec
    composeToDayOfWeek(listOfRanges) shouldBe IllegalValue("0-4,0-99/2 is not a valid day of week value").invalidNec
    composeToDayOfWeek(month) shouldBe IllegalValue("Nov is not a valid day of week value").invalidNec
  }

  "mainFunction" should "return a CronLine if all the validations are Ok" in {
    val line1 = "*/15 0 1,15 * 1-5 /usr/bin/find"
    val line2 = "59 23 * * 0-7 /usr/bin/find"
    val line3 = "10-45 15-19/2 15 10 5 /usr/bin/find"
    val line4 = "10-45/15 */3 1-15/5 */2 * /usr/bin/find"

    // "*/15 0 1,15 * 1-5 /usr/bin/find"
    mainFunction(line1) shouldBe
      Right(
        CronLine(
          Minute(List(0,15,30,45)),
          Hour(List(0)),
          DayOfMonth(List(1,15)),
          Month((1 to 12).toList),
          DayOfWeek((1 to 5).toList),
          Command("/usr/bin/find")
        )
      )

    // "59 23 * * 0-7 /usr/bin/find"
    mainFunction(line2) shouldBe
      Right(
        CronLine(
          Minute(List(59)),
          Hour(List(23)),
          DayOfMonth((1 to 31).toList),
          Month((1 to 12).toList),
          DayOfWeek((0 to 7).toList),
          Command("/usr/bin/find")
        )
      )

    // "10-45 15-19/2 15 10 5 /usr/bin/find"
    mainFunction(line3) shouldBe
      Right(
        CronLine(
          Minute((10 to 45).toList),
          Hour(List(16,18)),
          DayOfMonth(List(15)),
          Month(List(10)),
          DayOfWeek(List(5)),
          Command("/usr/bin/find")
        )
      )

    // "10-45/15 */3 1-15/5 */2 * /usr/bin/find"
    mainFunction(line4) shouldBe
      Right(
        CronLine(
          Minute(List(15,30,45)),
          Hour(List(0,3,6,9,12,15,18,21)),
          DayOfMonth(List(5,10,15)),
          Month(List(2,4,6,8,10,12)),
          DayOfWeek((0 to 7).toList),
          Command("/usr/bin/find")
        )
      )
  }

  it should "return an error if some validations fail" in {
    val line = "*/0 50 1,32 dec 1-8 /usr/bin/find"

    mainFunction(line) shouldBe
      Left(
          Chain(
            InvalidFormat("*/0 is not in a valid cron field format"),
            IllegalValue("50 is not a valid hour value"),
            IllegalValue("1,32 is not a valid day of month value"),
            IllegalValue("1-8 is not a valid day of week value")
          )
      )
  }
}
