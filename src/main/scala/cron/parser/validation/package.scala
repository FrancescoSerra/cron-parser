package cron.parser

import cats.data.{Kleisli, ValidatedNec}
import cats.implicits._
import RegExes._

package object validation {
  import cats.instances.list._, cats.syntax.list._
  import cats.syntax.traverse._

  /*
  * takes a String and tries to validate it into one of the possible cron fields formats
  * returns a FieldType which represents the type of format into which the string was parsed
  * Either context which represents a failure in recognising the type of FieldType, or the FieldType itself
  * */
  val validateFormat: Result[String,FieldType] = Kleisli { field =>
    lazy val validateIn:  String => Either[InvalidFormat,FieldType] = {
      case validEntry(entry) => Entry(entry.toInt).asRight
      case validAsterisk(step) =>
        Option[String](step).map(_.toInt) match {
          case some@Some(s) if s > 0 => Asterisk(some).asRight
          case Some(s) => InvalidFormat(s"*/$s is not in a valid cron field format").asLeft
          case _ => Asterisk(None).asRight
        }
      case validRange(start, end, step) =>
        val initVal = start.toInt
        val endVal = end.toInt
        if (endVal > initVal && (step != "0")) Range(initVal, endVal, Option[String](step).map(_.toInt)).asRight
        else InvalidFormat(s"Invalid range $initVal-$endVal${Option(step).map(v => s"/$v").getOrElse("")}").asLeft
      case validList(list) => ListOfEntries(list.split(",").toList.map(e => Entry(e.toInt))).asRight
      case validListOfRanges(listOfRanges) =>
        listOfRanges.split(",").toList.collect(validateIn(_)).traverse {
          case Right(rangeable: Rangeable) => rangeable.validNec[InvalidFormat]
          case Right(other) => other.invalidNec[Rangeable]
          case Left(invalidFormat) => invalidFormat.invalidNec[Rangeable]
        }.toEither.map(ListOfRanges).leftMap(invalid => InvalidFormat(invalid.toList.mkString))
      case validDay(day) => LiteralDay(day).asRight
      case validMonth(month) => LiteralMonth(month).asRight
      case token => InvalidFormat(s"$token is not in a valid cron field format").asLeft
    }

    validateIn(field)
  }

/*
* takes:
*   - min: Int, the lower bound of the set of possible values for that cron field
*   - max: Int, the upper bound of the set of possible values for that cron field
*   - stringRepr: String, the name of the cron field that's being validated
* returns the validate field
* Either context which represents the failure in validating the values for that field, or the field itself
* Requires an instance of Buildable in scope, in order to build an instance of the field - represented by the generic type parameter A
* */
  def validate[A: Buildable](min: Int, max: Int, stringRepr: String): ValidatedTo[A] = Kleisli { field =>
    (field match {
      case Entry(value) if value >= min && value <= max => List(value).asRight
      case Asterisk(maybeStep) =>
        maybeStep.map(step => (min to max).toList.filter(a => a % step == 0 || a == 0)).getOrElse((min to max).toList).asRight
      case Range(start, end, maybeStep) if start >= min && end <= max =>
        maybeStep.map(step => (start to end).toList.filter(a => a % step == 0 || a == 0)).getOrElse((start to end).toList).asRight
      case ListOfEntries(entries) if entries.map(_.value).forall(e => e >= min && e <= max) =>
        entries.map(_.value).distinct.sorted.asRight
      case ListOfRanges(ranges) if ranges.forall(r => r.start >= min && r.end <= max) =>
        ranges.flatMap(range =>
          range.maybeStep.map(step => (range.start to range.end).toList.filter(a => a % step == 0 || a == 0)).getOrElse((range.start to range.end).toList)
        ).distinct.sorted.asRight
      case illegalValue => IllegalValue(s"$illegalValue is not a valid $stringRepr value").asLeft
    }).map(Buildable[A].build)
  }


  /*
  * validates the minute field length
  * */
  val validateMinuteLength: Result[FieldType,Minute] = validate(0,59,"minute")

  /*
  * validates the hour field length
  * */
  val validateHourLength: Result[FieldType,Hour] = validate(0,23,"hour")

  /*
  * validates the day of month field
  * */
  val validateDayOfMonth: Result[FieldType,DayOfMonth] = validate(1,31,"day of month")
}
