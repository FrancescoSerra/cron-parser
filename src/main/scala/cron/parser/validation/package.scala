package cron.parser

import cats.data.Kleisli
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
    def validateIn(input: String): Either[InvalidFormat,FieldType] = {
      println("my input is " + input)
      input match {
        case validEntry(entry) => Right(Entry(entry.toInt))
        case validAsterisk(step) =>
          Option[String](step).map(_.toInt) match {
            case some@Some(s) if s > 0 => Right(Asterisk(some))
            case Some(s) => Left(InvalidFormat(s"*/$s is not in a valid cron field format"))
            case _ => Right(Asterisk(None))
          }
        case validRange(start, end, step) =>
          val initVal = start.toInt
          val endVal = end.toInt
          if (endVal > initVal && (step != "0")) Right(Range(initVal, endVal, Option[String](step).map(_.toInt)))
          else Left(InvalidFormat(s"Invalid range $initVal-$endVal${Option(step).map(v => s"/$v").getOrElse("")}"))
        case validList(list) => Right(ListOfEntries(list.split(",").toList.map(e => Entry(e.toInt))))
        case validListOfRanges(listOfRanges) => {
          println(s"*********** list of ranges: $listOfRanges")
          listOfRanges.split(",").toList.collect(validateIn(_)).map {
            case Right(rangeable: Rangeable) => rangeable.validNec[InvalidFormat]
            case Right(other) => other.invalidNec[Rangeable]
            case Left(invalidFormat) => invalidFormat.invalidNec[Rangeable]
          }.sequence.toEither.map(ListOfRanges).leftMap(invalid => InvalidFormat(invalid.toList.mkString))
        }
        case validDay(day) => Right(LiteralDay(day))
        case validMonth(month) => Right(LiteralMonth(month))
        case token => Left(InvalidFormat(s"$token is not in a valid cron field format"))
      }
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
  def validate[A: Buildable](min: Int, max: Int, stringRepr: String): ValidatedTo[A] = Kleisli( field => {
    val impl = implicitly[Buildable[A]]
    field match {
      case Entry(value) if value >= min && value <= max => Right(impl(List(value)))
      case Asterisk(maybeStep) =>
        Right(impl(maybeStep.map(step => (min to max).toList.filter(a => a % step == 0 || a == 0)).getOrElse((min to max).toList)))
      case Range(start, end, maybeStep) if start >= min && end <= max =>
        Right(impl(maybeStep.map(step => (start to end).toList.filter(a => a % step == 0 || a == 0)).getOrElse((start to end).toList)))
      case l@ListOfEntries(entries) =>
        if (entries.map(_.value).forall(e => e >= min && e <= max)) Right(impl(entries.map(_.value).toSet.toList.sorted))
        else Left(IllegalValue(s"$l is not a valid $stringRepr value"))
      case ListOfRanges(ranges) =>
        if (ranges.forall(r => r.start >= min && r.end <= max))
          Right(impl(ranges.flatMap(range =>
            range.maybeStep.map(step => (range.start to range.end).toList.filter(a => a % step == 0 || a == 0)).getOrElse((range.start to range.end).toList)
          ).distinct.sorted))
        else Left(IllegalValue(s"${ranges.mkString(",")} is not a valid $stringRepr value"))
      case illegalValue => Left(IllegalValue(s"$illegalValue is not a valid $stringRepr value"))
    }
  })

  /*
  * validates the minute field length
  * */
  val validateMinuteLength: Result[FieldType,Minute] = validate[Minute](0,59,"minute")

  /*
  * validates the hour field length
  * */
  val validateHourLength: Result[FieldType,Hour] = validate[Hour](0,23,"hour")

  /*
  * validates the day of month field
  * */
  val validateDayOfMonth: Result[FieldType,DayOfMonth] = validate[DayOfMonth](1,31,"day of month")
}
