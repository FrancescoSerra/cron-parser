package cron.parser

object RegExes {
  /*
  * collection of all the regexes used for parsing the input cron line
  * */
  val lineRegex = "(.*?)\\s+(.*?)\\s+(.*?)\\s+(.*?)\\s+(.*?)\\s+(.*)".r

  val listOfDays = List(Sun,Mon,Tue,Wed,Thu,Fri,Sat)
  val listOfDaysSundayTwice = listOfDays appended Sun
  val listOfMonths = List(
    Jan,
    Feb,
    Mar,
    Apr,
    May,
    Jun,
    Jul,
    Aug,
    Sep,
    Oct,
    Nov,
    Dec)

  val validEntry = "(\\d(?:\\d)?)".r
  val validAsterisk = "\\*(?:/(\\d\\d?))?".r
  val validRange = "(\\d\\d?)-(\\d\\d?)(?:/(\\d\\d?))?".r
  val validList = "(\\d(?:\\d)?(?:,\\d(?:\\d)?)*)".r
  val validListOfRanges = """((?:\d\d?-\d\d?(?:/\d\d?)?|\d\d?)(?:,(?:\d\d?-\d\d?(?:/\d\d?)?|\d\d?))+)""".r
  val validDay = s"(?i)((?:${listOfDays.mkString("|")})(?:,(?:${listOfDays.mkString("|")}))*)".r
  val validMonth = s"(?i)((?:${listOfMonths.mkString("|")})(?:,(?:${listOfMonths.mkString("|")}))*)".r
}
