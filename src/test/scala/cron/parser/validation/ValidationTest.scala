package cron.parser.validation

import cron.parser.{Asterisk, DayOfMonth, Entry, Hour, IllegalValue, InvalidFormat, ListOfEntries, ListOfRanges, LiteralDay, LiteralMonth, Minute, Range}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidationTest extends AnyFlatSpec with Matchers {

  "validateFormat" should "return a success if the format of a token is valid" in {
    val maybeElement0 = "1"
    val maybeElement1 = "*"
    val maybeElement2 = "8-11"
    val maybeElement3 = "1,2,5,9,11"
    val maybeElement4 = "0-4,8-12,13-17"
    val maybeElement4mixed = "0-4,5,6,8-12"
    val maybeElement5 = "0-23/2"
    val maybeElement6 = "*/2"
    val maybeElement7 = "Tue"
    val maybeElement8 = "Nov"

    validateFormat(maybeElement0) shouldBe Right(Entry(1))
    validateFormat(maybeElement1) shouldBe Right(Asterisk(None))
    validateFormat(maybeElement2) shouldBe Right(Range(8,11,None))
    validateFormat(maybeElement3) shouldBe Right(ListOfEntries(List(Entry(1),Entry(2),Entry(5),Entry(9),Entry(11))))
    validateFormat(maybeElement4) shouldBe Right(ListOfRanges(List(Range(0,4),Range(8,12),Range(13,17))))
    validateFormat(maybeElement4mixed) shouldBe Right(ListOfRanges(List(Range(0,4),Entry(5),Entry(6),Range(8,12))))
    validateFormat(maybeElement5) shouldBe Right(Range(0,23,Some(2)))
    validateFormat(maybeElement6) shouldBe Right(Asterisk(Some(2)))
    validateFormat(maybeElement7) shouldBe Right(LiteralDay("Tue"))
    validateFormat(maybeElement8) shouldBe Right(LiteralMonth("Nov"))
  }

  it should "return a failure if the token is not in a valid cron field format" in {
    validateFormat("foo") shouldBe Left(InvalidFormat("foo is not in a valid cron field format"))
    validateFormat("10-5") shouldBe Left(InvalidFormat("Invalid range 10-5"))
    validateFormat("5-10/0") shouldBe Left(InvalidFormat("Invalid range 5-10/0"))
  }

  "validateMinuteLength" should "return a valid list of minutes if the field contains legal values" in {
    val entry = Entry(1)
    val asterisk = Asterisk(None)
    val range = Range(8,11,None)
    val listOfEntries = ListOfEntries(List(Entry(1),Entry(2),Entry(5),Entry(9),Entry(11)))
    val listOfRanges = ListOfRanges(List(Range(0,4),Range(8,12)))
    val rangeWithStep = Range(0,23,Some(2))
    val asteriskWithStep = Asterisk(Some(2))

    validateMinuteLength(entry) shouldBe Right(Minute(List(1)))
    validateMinuteLength(asterisk) shouldBe Right(Minute((0 to 59).toList))
    validateMinuteLength(range) shouldBe Right(Minute((8 to 11).toList))
    validateMinuteLength(listOfEntries) shouldBe Right(Minute(List(1,2,5,9,11)))
    validateMinuteLength(listOfRanges) shouldBe Right(Minute((0 to 4).toList ++ (8 to 12).toList))
    validateMinuteLength(rangeWithStep) shouldBe Right(Minute(List(0,2,4,6,8,10,12,14,16,18,20,22)))
    validateMinuteLength(asteriskWithStep) shouldBe Right(Minute((0 to 59).toList.filter(_ % 2 == 0)))
  }

  it should "return a failure if the minute field contains illegal values" in {
    val entry = Entry(60)
    val range = Range(8,80,None)
    val listOfEntries = ListOfEntries(List(Entry(61),Entry(62)))
    val listOfRanges = ListOfRanges(List(Range(0,4,None),Range(0,99,Some(2))))
    val day = LiteralDay("Tue")
    val month = LiteralMonth("Nov")

    validateMinuteLength(entry) shouldBe Left(IllegalValue("60 is not a valid minute value"))
    validateMinuteLength(range) shouldBe Left(IllegalValue("8-80 is not a valid minute value"))
    validateMinuteLength(listOfEntries) shouldBe Left(IllegalValue("61,62 is not a valid minute value"))
    validateMinuteLength(listOfRanges) shouldBe Left(IllegalValue("0-4,0-99/2 is not a valid minute value"))
    validateMinuteLength(day) shouldBe Left(IllegalValue("Tue is not a valid minute value"))
    validateMinuteLength(month) shouldBe Left(IllegalValue("Nov is not a valid minute value"))
  }

  "validateHourLength" should "return a valid list of hours if the field contains legal values" in {
    val entry = Entry(1)
    val asterisk = Asterisk(None)
    val range = Range(8,11,None)
    val listOfEntries = ListOfEntries(List(Entry(1),Entry(2),Entry(5),Entry(9),Entry(11)))
    val listOfRanges = ListOfRanges(List(Range(0,4),Range(8,12)))
    val rangeWithStep = Range(0,23,Some(2))
    val asteriskWithStep = Asterisk(Some(2))

    validateHourLength(entry) shouldBe Right(Hour(List(1)))
    validateHourLength(asterisk) shouldBe Right(Hour((0 to 23).toList))
    validateHourLength(range) shouldBe Right(Hour((8 to 11).toList))
    validateHourLength(listOfEntries) shouldBe Right(Hour(List(1,2,5,9,11)))
    validateHourLength(listOfRanges) shouldBe Right(Hour((0 to 4).toList ++ (8 to 12).toList))
    validateHourLength(rangeWithStep) shouldBe Right(Hour(List(0,2,4,6,8,10,12,14,16,18,20,22)))
    validateHourLength(asteriskWithStep) shouldBe Right(Hour((0 to 23).toList.filter(_ % 2 == 0)))
  }

  it should "return a failure if the hour field contains illegal values" in {
    val entry = Entry(60)
    val range = Range(8,80,None)
    val listOfEntries = ListOfEntries(List(Entry(61),Entry(62)))
    val listOfRanges = ListOfRanges(List(Range(0,4,None),Range(0,99,Some(2))))
    val day = LiteralDay("Tue")
    val month = LiteralMonth("Nov")

    validateHourLength(entry) shouldBe Left(IllegalValue("60 is not a valid hour value"))
    validateHourLength(range) shouldBe Left(IllegalValue("8-80 is not a valid hour value"))
    validateHourLength(listOfEntries) shouldBe Left(IllegalValue("61,62 is not a valid hour value"))
    validateHourLength(listOfRanges) shouldBe Left(IllegalValue("0-4,0-99/2 is not a valid hour value"))
    validateHourLength(day) shouldBe Left(IllegalValue("Tue is not a valid hour value"))
    validateHourLength(month) shouldBe Left(IllegalValue("Nov is not a valid hour value"))
  }

  "validateDayOfMonth" should "return a valid list of days if the field contains legal values" in {
    val entry = Entry(1)
    val asterisk = Asterisk(None)
    val range = Range(8,11,None)
    val listOfEntries = ListOfEntries(List(Entry(1),Entry(2),Entry(5),Entry(9),Entry(11)))
    val listOfRanges = ListOfRanges(List(Range(1,4),Range(8,12)))
    val rangeWithStep = Range(1,23,Some(2))
    val asteriskWithStep = Asterisk(Some(2))

    validateDayOfMonth(entry) shouldBe Right(DayOfMonth(List(1)))
    validateDayOfMonth(asterisk) shouldBe Right(DayOfMonth((1 to 31).toList))
    validateDayOfMonth(range) shouldBe Right(DayOfMonth((8 to 11).toList))
    validateDayOfMonth(listOfEntries) shouldBe Right(DayOfMonth(List(1,2,5,9,11)))
    validateDayOfMonth(listOfRanges) shouldBe Right(DayOfMonth((1 to 4).toList ++ (8 to 12).toList))
    validateDayOfMonth(rangeWithStep) shouldBe Right(DayOfMonth(List(2,4,6,8,10,12,14,16,18,20,22)))
    validateDayOfMonth(asteriskWithStep) shouldBe Right(DayOfMonth((1 to 31).toList.filter(_ % 2 == 0)))
  }

  it should "return a failure if the day of month field contains illegal values" in {
    val entry = Entry(60)
    val range = Range(8,80,None)
    val listOfEntries = ListOfEntries(List(Entry(61),Entry(62)))
    val listOfRanges = ListOfRanges(List(Range(0,4,None),Range(0,99,Some(2))))
    val day = LiteralDay("Tue")
    val month = LiteralMonth("Nov")

    validateDayOfMonth(entry) shouldBe Left(IllegalValue("60 is not a valid day of month value"))
    validateDayOfMonth(range) shouldBe Left(IllegalValue("8-80 is not a valid day of month value"))
    validateDayOfMonth(listOfEntries) shouldBe Left(IllegalValue("61,62 is not a valid day of month value"))
    validateDayOfMonth(listOfRanges) shouldBe Left(IllegalValue("0-4,0-99/2 is not a valid day of month value"))
    validateDayOfMonth(day) shouldBe Left(IllegalValue("Tue is not a valid day of month value"))
    validateDayOfMonth(month) shouldBe Left(IllegalValue("Nov is not a valid day of month value"))
  }
}
