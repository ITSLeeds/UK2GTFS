#' get bank holidays
#' @param url url to ics file
#' @export
get_bank_holidays <- function(url = "https://www.gov.uk/bank-holidays/england-and-wales.ics"){
  download.file(url = url, destfile = "bankholidays.ics")
  cal <- as.data.frame(calendar::ic_read("bankholidays.ics"))
  file.remove("bankholidays.ics")
  names(cal) <- c("start","date","name","UID","SEQUENCE","DTSTAMP")
  cal <- cal[,c("name","date")]
  cal$name[cal$name == "Summer bank holiday"] <- "LateSummerBankHolidayNotScotland"
  cal$name[cal$name == "Spring bank holiday"] <- "SpringBank"
  cal$name[cal$name == "New Yearâ€™s Day"] <- "NewYearsDay"
  cal$name[cal$name == "Christmas Day"] <- "ChristmasDay"
  cal$name[cal$name == "Boxing Day"] <- "BoxingDay"

  return(cal)
}
