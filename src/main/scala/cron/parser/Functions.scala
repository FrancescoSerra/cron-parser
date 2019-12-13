package cron.parser

import cats.implicits._
import cats.data.NonEmptyChain

import RegExes._
import validation._

object Functions {

  /*
  * takes a String and tries to parse it into a LineTokens instance that represents the tokens in the input line
  * returns the LineTokens instance
  * Either context that represents a failure in parsing the line, or the LineTokens
  * */
  val parseLine: String => Either[ParsingError,LineTokens] = {
    case lineRegex(minute, hour, dayOfMonth, month, dayOfWeek, command) => Right(LineTokens(minute,hour,dayOfMonth,month,dayOfWeek,command))
    case input => Left(ParsingError(s"input couldn't be parsed into the expected tokens: $input"))
  }

  /*
  * composes validation of the format and of the minute field value
  * */
  val composeToMinute: String => ValidationResult[Minute] = entry =>
    (validateFormat andThen validateMinuteLength).run(entry).toValidatedNec

  /*
  * composes validation of the format and of the hour field value
  * */
  val composeToHour: String => ValidationResult[Hour] = entry =>
    (validateFormat andThen validateHourLength).run(entry).toValidatedNec

  /*
  * composes validation of the format and of the day of month field value
  * */
  val composeToDayOfMonth: String => ValidationResult[DayOfMonth] = entry =>
    (validateFormat andThen validateDayOfMonth).run(entry).toValidatedNec

  /*
  * composes validation of the format and of the month field value
  * */
  val composeToMonth: String => ValidationResult[Month] = entry => {
    validateFormat(entry) match {
      case Left(error) => error.invalidNec[Month]
      case Right(m) => m match {
        case LiteralMonth(month) => Month(listOfMonths.zipWithIndex.filter { case (m, _) => m.equalsIgnoreCase(month) }.map(_._2 + 1)).validNec
        case other => validate[Month](1, 12, "month").run(other).toValidatedNec
      }
    }
  }

  /*
  * composes validation of the format and of the day of week field value
  * */
  val composeToDayOfWeek: String => ValidationResult[DayOfWeek] = entry =>
    validateFormat(entry) match {
      case Left(error) => error.invalidNec[DayOfWeek]
      case Right(d) => d match {
        case LiteralDay(day) => DayOfWeek(listOfDays.zipWithIndex.filter { case (d,_) => d.equalsIgnoreCase(day)}.map(_._2)).validNec
        case other => validate[DayOfWeek](0,7,"day of week").run(other).toValidatedNec
      }
    }

  /*
  * generates a Validated wrapper around any value in the command field
  * */
  val composeToCommand: String => ValidationResult[Command] = entry => Command(entry).validNec

  /*
  * composes all the above functions and returns a CronLine instance or the collection of all the errors encountered during validation
  * */
  val mainFunction: String => ErrorOr[CronLine] = entry =>
    for {
      tokenisedLine <- parseLine(entry).leftMap(e => NonEmptyChain.one(e))
      LineTokens(minute,hour,dayOfMonth,month,dayOfWeek,command) = tokenisedLine
      cronLine <- (
        composeToMinute(minute),
        composeToHour(hour),
        composeToDayOfMonth(dayOfMonth),
        composeToMonth(month),
        composeToDayOfWeek(dayOfWeek),
        composeToCommand(command)
      ).mapN(CronLine.apply).toEither
    } yield cronLine
}
