#' get bank holidays
#'
#' Donload the UK bank holidays
#' @param url url to ics file
#' @export
#'
get_bank_holidays <- function(url = "https://www.gov.uk/bank-holidays/england-and-wales.ics") {
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

  return(cal)
}
