[![Build Status](https://travis-ci.com/FrancescoSerra/cron-parser.svg?branch=master)](https://travis-ci.com/FrancescoSerra/cron-parser)

# Cron expressions parser

The goal of this project is to build a cron expression parser that can parse a cron entry string, print a table to the console and expand each field to show the times at which it will run.

## Implementation

I preferred a functional approach to implement the business logic, the parts of which are expressed as pure functions which can be composed in order to return the expected values. 
Hence, they operate in an Either[A,B] context to represent potential failures. Where it made sense, to provide parallel validation of the cron line tokens, they operate in a ValidatedNec[A,B] context.

## Build

In a terminal, execute `sbt assembly` from the root of the project, that will generate an executable fat jar, that you'll find in `target/scala-2.13/cron-parser-assembly-<version>.jar`.

## Run

Run the application in a terminal using the following command line: `scala target/scala-2.13/cron-parser-assembly-0.1.jar <input cron line>` where <input cron line> is the line that's going to be 
evaluated.
Alternatively, you might use the command `java -jar target/scala-2.13/cron-parser-assembly-<version>.jar <input cron line>` if you incur issues running it using the scala executable wrapper.

For example, `scala target/scala-2.13/cron-parser-assembly-0.1.jar "25 6 * * * root /usr/sbin/anacron"` will generate the following output:

|    field  | time values                                                                                   |
| --- | --- |
|minute         |25|
|hour           |6|
|day of month   |1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31|
|month          |1 2 3 4 5 6 7 8 9 10 11 12|
|day of week    |0 1 2 3 4 5 6 7|
|command        |root /usr/sbin/anacron|


## Test

To execute the test suite of the application, run `sbt test` in a terminal from the root of the project.
