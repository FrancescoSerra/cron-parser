package cron

import cats.data.{Kleisli, NonEmptyChain, ValidatedNec}

package object parser {
  /* domain entities */
  trait Buildable[T] {
    def apply(list: List[Int]): T
  }

  type ValidatedTo[A] = Result[FieldType,A]

  case class LineTokens(
                         maybeMinute: String,
                         maybeHour: String,
                         maybeDayOfMonth: String,
                         maybeMonth: String,
                         maybeDayOfWeek: String,
                         maybeCommand: String
                       )


  sealed trait FieldType
  sealed trait Rangeable {
    def start: Int
    def end: Int
    def maybeStep: Option[Int]
  }
  final case class Entry(value: Int) extends FieldType with Rangeable {
    val start = value
    val end = value
    val maybeStep = None
    override def toString: String = value.toString
  }
  final case class Asterisk(maybeStep: Option[Int] = None) extends FieldType
  final case class Range(start: Int, end: Int, maybeStep: Option[Int] = None) extends FieldType with Rangeable {
    override def toString: String = s"$start-$end" + maybeStep.map(v => s"/$v").getOrElse("")
  }
  final case class ListOfEntries(list: List[Entry]) extends FieldType {
    override def toString: String = list.mkString(",")
  }
  final case class ListOfRanges(list: List[Rangeable]) extends FieldType
  final case class LiteralDay(value: String) extends FieldType {
    override def toString: String = value
  }
  final case class LiteralMonth(value: String) extends FieldType {
    override def toString: String = value
  }

  /* Field hierarchy */
  sealed trait Field extends Any

  final case class Minute(listOfMinutes: List[Int]) extends Field {
    override def toString: String = listOfMinutes.mkString(" ")
  }
  object Minute {
    implicit val minuteBuildable: Buildable[Minute] = (list: List[Int]) => Minute(list)
  }
  final case class Hour(listOfHours: List[Int]) extends Field {
    override def toString: String = listOfHours.mkString(" ")
  }
  object Hour {
    implicit val hourBuildable: Buildable[Hour] = (list: List[Int]) => Hour(list)
  }
  final case class DayOfMonth(listOfDays: List[Int]) extends Field {
    override def toString: String = listOfDays.mkString(" ")
  }
  object DayOfMonth {
    implicit val dayOfMonthBuildable: Buildable[DayOfMonth] = (list: List[Int]) => DayOfMonth(list)
  }
  final case class Month(listOfMonths: List[Int]) extends Field {
    override def toString: String = listOfMonths.mkString(" ")
  }
  object Month {
    implicit val monthBuildable: Buildable[Month] = (list: List[Int]) => Month(list)
  }
  final case class DayOfWeek(listOfDays: List[Int]) extends Field {
    override def toString: String = listOfDays.mkString(" ")
  }
  object DayOfWeek {
    implicit val dayOfWeekBuildable: Buildable[DayOfWeek] = (list: List[Int]) => DayOfWeek(list)
  }
  final case class Command(value: String) extends AnyVal with Field {
    override def toString: String = value
  }

  final case class CronLine(
                           minute: Minute,
                           hour: Hour,
                           dayOfMonth: DayOfMonth,
                           month: Month,
                           dayOfWeek: DayOfWeek,
                           command: Command
                           )

  /* Error hierarchy */
  sealed trait Error {
    val message: String
    override def toString: String = message
  }

  final case class ParsingError(message: String) extends Error
  final case class InvalidFormat(message: String) extends Error
  final case class IllegalValue(message: String) extends Error

  /* functional type aliases */
  type ValidationResult[A] = ValidatedNec[Error, A]
  type Result[A,B] = Kleisli[Either[Error,*],A,B]
  type ErrorOr[A] = Either[NonEmptyChain[Error],A]

  sealed trait Day extends Product with Serializable
  final case object Sun extends Day
  final case object Mon extends Day
  final case object Tue extends Day
  final case object Wed extends Day
  final case object Thu extends Day
  final case object Fri extends Day
  final case object Sat extends Day

  sealed trait MonthLiteral extends Product with Serializable
  final case object Jan extends MonthLiteral
  final case object Feb extends MonthLiteral
  final case object Mar extends MonthLiteral
  final case object Apr extends MonthLiteral
  final case object May extends MonthLiteral
  final case object Jun extends MonthLiteral
  final case object Jul extends MonthLiteral
  final case object Aug extends MonthLiteral
  final case object Sep extends MonthLiteral
  final case object Oct extends MonthLiteral
  final case object Nov extends MonthLiteral
  final case object Dec extends MonthLiteral
}
