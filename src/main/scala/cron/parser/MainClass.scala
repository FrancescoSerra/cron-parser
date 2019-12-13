package cron.parser

import Functions._

class MainClass extends App {
  val optInput = args.headOption

  optInput match {
    case Some(line) => mainFunction(line) match {
      case Right(cronLine) =>
        printf("%-14s %s\n", "minute", cronLine.minute)
        printf("%-14s %s\n", "hour", cronLine.hour)
        printf("%-14s %s\n", "day of month", cronLine.dayOfMonth)
        printf("%-14s %s\n", "month", cronLine.month)
        printf("%-14s %s\n", "day of week", cronLine.dayOfWeek)
        printf("%-14s %s\n", "command", cronLine.command)
      case Left(errorChain) => println(errorChain.toNonEmptyList.toList.mkString(","))
    }
    case None => println("Please provide a cron line to interpret")
  }
}

object Main extends MainClass
