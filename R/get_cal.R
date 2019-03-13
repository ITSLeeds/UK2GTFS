# get bank holidays

get_bank_holidays <- function(url = "https://www.gov.uk/bank-holidays/england-and-wales.ics"){
  download.file(url = url, destfile = "bankholidays.ics")
  cal <- calendar::ic_read("bankholidays.ics")
  file.remove("bankholidays.ics")
  names(cal) <- c("start","date","name","UID","SEQUENCE","DTSTAMP")
  cal <- cal[,c("name","date")]
  cal$name[cal$name == "Summer bank holiday"] <- "LateSummerBankHolidayNotScotland"
  cal$name[cal$name == "Spring bank holiday"] <- "SpringBank"
  return(cal)
}
