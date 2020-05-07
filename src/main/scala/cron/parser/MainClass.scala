package cron.parser

import Functions._
import cats.data.{NonEmptyChain, NonEmptyList}
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

sealed trait RunResult extends Product with Serializable
case object SuccessResult extends RunResult
case object ErrorResult extends RunResult

class MainClass extends IOApp {
  type Compound = Either[String,NonEmptyList[(String,Field)]]

  val translate: Option[String] => Compound = input =>
    input.map(line => mainFunction(line).map(cronLine => {
      import cronLine._
      NonEmptyList.of(
        ("minute", minute),
        ("hour", hour),
        ("day of month", dayOfMonth),
        ("month", month),
        ("day of week", dayOfWeek),
        ("command", command)
      )}
    ).leftMap(_.toList.mkString(","))).getOrElse("Please provide a cron line to interpret".asLeft)

  val printRes: Compound => IO[RunResult] = compound =>
    compound.map { vals =>
      vals.toList.map { case (name, field) =>
        IO(printf("%-14s %s\n", name, field))
      }.sequence *> IO.pure(SuccessResult)
    }.valueOr { err =>
      IO(println(err)) *> IO.pure(ErrorResult)
    }

  override def run(args: List[String]): IO[ExitCode] = {
    (translate andThen printRes)(args.headOption).map {
      case ErrorResult => ExitCode.Error
      case SuccessResult => ExitCode.Success
    }
  }
}

object Main extends MainClass
