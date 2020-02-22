#' Get Bank Holiday Calendar
#' TODO: Get scottosh holidays
#' Downloads and formats the bank holiday calendar for use with TransXchange data.
#' @param url url to ics file
#' @export
#'
get_bank_holidays <- function(url = "https://www.gov.uk/bank-holidays/england-and-wales.ics") {
  message("Scottish holidays not yet supported")
  utils::download.file(url = url, destfile = "bankholidays.ics")
  cal <- as.data.frame(calendar::ic_read("bankholidays.ics"))
  file.remove("bankholidays.ics")
  names(cal) <- c("start", "date", "name", "UID", "SEQUENCE", "DTSTAMP")
  cal <- cal[, c("name", "date")]
  cal$name[cal$name == "Summer bank holiday"] <- "LateSummerBankHolidayNotScotland"
  cal$name[cal$name == "Spring bank holiday"] <- "SpringBank"
  cal$name[cal$name == "New Year\U00E2\U20AC\U2122s Day"] <- "NewYearsDay"
  cal$name[cal$name == "Christmas Day"] <- "ChristmasDay"
  cal$name[cal$name == "Boxing Day"] <- "BoxingDay"
  cal$name[cal$name == "Good Friday"] <- "GoodFriday"

  return(cal)
}
