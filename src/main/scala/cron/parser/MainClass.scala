package cron.parser

import Functions._
import cats.data.NonEmptyChain
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.either._

sealed trait RunResult extends Product with Serializable
case object SuccessResult extends RunResult
case object ErrorResult extends RunResult

class MainClass extends IOApp {
  type Compound = Either[String,NonEmptyChain[(String,Field)]]

  val mainFunc: Option[String] => Compound = input =>
    input.map(line => mainFunction(line).map { cronLine =>
      NonEmptyChain(
        ("minute", cronLine.minute),
        ("hour", cronLine.hour),
        ("day of month", cronLine.dayOfMonth),
        ("month", cronLine.month),
        ("day of week", cronLine.dayOfWeek),
        ("command", cronLine.command)
      )
    }.leftMap(_.toNonEmptyList.toList.mkString(","))).getOrElse("Please provide a cron line to interpret".asLeft)

  val printRes: Compound => IO[RunResult] = {
    case Right(vals) => for {
      _ <- IO(vals.toNonEmptyList.toList.foreach { case (name,field) =>
              printf("%-14s %s\n", name, field)
            })
    } yield SuccessResult

    case Left(err) => for {
      _ <- IO(println(err))
    } yield ErrorResult
  }


  override def run(args: List[String]): IO[ExitCode] = {
    (mainFunc andThen printRes)(args.headOption).map {
      case ErrorResult => ExitCode.Error
      case SuccessResult => ExitCode.Success
    }
  }
}

object Main extends MainClass
