#' Get Bank Holiday Calendar
#'
#' Downloads and formats the bank holiday calendar for use with TransXchange
#' data.
#' @param url_ew url to ics file for England and Wales
#' @param url_scot url to ics file for Scotland
#' @return data frame
#' @details TransXchange records bank holidays by name (e.g. Christmas Day),
#'   some UK bank holidays move around, so this function downloads the official
#'   bank holiday calendar. The offical feed only covers a short period of time
#'   so this may not be suitable for converting files from the past / future.
#' @export
#'
get_bank_holidays <- function(url_ew = "https://www.gov.uk/bank-holidays/england-and-wales.ics",
                              url_scot = "https://www.gov.uk/bank-holidays/scotland.ics") {
  message("Scottish holidays are downloaded but not properly supported")
  dir.create(file.path(tempdir(), "UK2GTFS"))
  utils::download.file(
    url = url_ew,
    destfile = file.path(
      tempdir(), "UK2GTFS",
      "bankholidays_EW.ics"
    ),
    quiet = TRUE
  )
  utils::download.file(
    url = url_scot,
    destfile = file.path(
      tempdir(), "UK2GTFS",
      "bankholidays_Scot.ics"
    ),
    quiet = TRUE
  )
  cal_ew <- as.data.frame(calendar::ic_read(file.path(
    tempdir(), "UK2GTFS",
    "bankholidays_EW.ics"
  )))
  cal_scot <- as.data.frame(calendar::ic_read(file.path(
    tempdir(), "UK2GTFS",
    "bankholidays_Scot.ics"
  )))
  names(cal_ew) <- c("start", "date", "name", "UID", "SEQUENCE", "DTSTAMP")
  names(cal_scot) <- c("start", "date", "name", "UID", "SEQUENCE", "DTSTAMP")
  cal_ew <- cal_ew[, c("name", "date")]
  cal_scot <- cal_scot[, c("name", "date")]

  # Remove duplicated days from scotland
  cal <- rbind(cal_ew, cal_scot)
  cal <- cal[!duplicated(cal$date), ]
  cal$EnglandWales <- cal$date %in% cal_ew$date
  cal$Scotland <- cal$date %in% cal_scot$date
  cal <- cal[order(cal$date), ]

  cal$name[cal$name == "Summer bank holiday"] <- "LateSummerBankHolidayNotScotland"
  cal$name[cal$name == "Spring bank holiday"] <- "SpringBank"
  cal$name[cal$name == "New Year\U00E2\U20AC\U2122s Day"] <- "NewYearsDay"
  cal$name[cal$name == "Christmas Day"] <- "ChristmasDay"
  cal$name[cal$name == "Boxing Day"] <- "BoxingDay"
  cal$name[cal$name == "Good Friday"] <- "GoodFriday"

  return(cal)
}
