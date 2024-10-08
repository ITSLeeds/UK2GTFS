#' Write GTFS
#'
#' Takes a list of data frames representing the GTFS format and saves them as GTFS
#' Zip file.
#'
#' @param gtfs named list of data.frames
#' @param folder folder to save the gtfs file to
#' @param name the name of the zip file without the .zip extension, default "gtfs"
#' @param stripComma logical, should commas be stripped from text, default = TRUE
#' @param stripTab logical, should tab be stripped from text, default = TRUE
#' @param stripNewline logical, should newline tag be stripped from text, default = TRUE
#' @param quote logical, should strings be quoted, default = FALSE, passed to `data.table::fwrite`
#' @export
#'
gtfs_write <- function(gtfs,
                       folder = getwd(),
                       name = "gtfs",
                       stripComma = TRUE,
                       stripTab = TRUE,
                       stripNewline = TRUE,
                       quote = FALSE) {
  if (stripComma) {
    for (i in seq_len(length(gtfs))) {
      gtfs[[i]] <- stripCommas(gtfs[[i]])
    }
  }

  if (stripTab) {
    for (i in seq_len(length(gtfs))) {
      gtfs[[i]] <- stripTabs(gtfs[[i]], stripNewline)
    }
  }


  #Format Dates

  if(inherits(gtfs$calendar$start_date, "Date")){
    gtfs$calendar$start_date <- format(gtfs$calendar$start_date, "%Y%m%d")
  }

  if(inherits(gtfs$calendar$end_date, "Date")){
    gtfs$calendar$end_date <- format(gtfs$calendar$end_date, "%Y%m%d")
  }

  if(inherits(gtfs$calendar_dates$date, "Date")){
    gtfs$calendar_dates$date <- format(gtfs$calendar_dates$date, "%Y%m%d")
  }

  #Format times
  if(inherits(gtfs$stop_times$arrival_time, "Period")){
    gtfs$stop_times$arrival_time <- period2gtfs(gtfs$stop_times$arrival_time)
  }

  if(inherits(gtfs$stop_times$departure_time, "Period")){
    gtfs$stop_times$departure_time <- period2gtfs(gtfs$stop_times$departure_time)
  }


  dir.create(paste0(tempdir(), "/gtfs_temp"))
  data.table::fwrite(gtfs$calendar, paste0(tempdir(), "/gtfs_temp/calendar.txt"), row.names = FALSE, quote = quote)
  if (nrow(gtfs$calendar_dates) > 0) {
    data.table::fwrite(gtfs$calendar_dates, paste0(tempdir(), "/gtfs_temp/calendar_dates.txt"), row.names = FALSE, quote = quote)
  }
  data.table::fwrite(gtfs$routes, paste0(tempdir(), "/gtfs_temp/routes.txt"), row.names = FALSE, quote = quote)
  data.table::fwrite(gtfs$stop_times, paste0(tempdir(), "/gtfs_temp/stop_times.txt"), row.names = FALSE, quote = quote)
  data.table::fwrite(gtfs$trips, paste0(tempdir(), "/gtfs_temp/trips.txt"), row.names = FALSE, quote = quote)
  data.table::fwrite(gtfs$stops, paste0(tempdir(), "/gtfs_temp/stops.txt"), row.names = FALSE, quote = quote)
  data.table::fwrite(gtfs$agency, paste0(tempdir(), "/gtfs_temp/agency.txt"), row.names = FALSE, quote = quote)
  if ("transfers" %in% names(gtfs)) {
    data.table::fwrite(gtfs$transfers, paste0(tempdir(), "/gtfs_temp/transfers.txt"), row.names = FALSE, quote = quote)
  }
  if ("shapes" %in% names(gtfs)) {
    data.table::fwrite(gtfs$shapes, paste0(tempdir(), "/gtfs_temp/shapes.txt"), row.names = FALSE, quote = quote)
  }
  zip::zipr(paste0(folder, "/", name, ".zip"), list.files(paste0(tempdir(), "/gtfs_temp"), full.names = TRUE), recurse = FALSE)
  unlink(paste0(tempdir(), "/gtfs_temp"), recursive = TRUE)
  message(paste0(folder, "/", name, ".zip"))
}


#' Strip Commas
#'
#' Remove commas from data frame
#'
#' @param df data frame
#' @noRd
#'
stripCommas <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "character")) {
      if(!all(validUTF8(x))){
        Encoding(x) <- "latin1"
        x <- enc2utf8(x)
      }
      x <- gsub(",", " ", x, fixed = TRUE)
    }
    return(x)
  })
  return(df)
}

#' Strip tabs
#'
#' Remove tabs from data frame
#'
#' @param df data frame
#' @param stripNewline logical
#' @noRd
#'
stripTabs <- function(df, stripNewline) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "character")) {
      if(!all(validUTF8(x))){
        Encoding(x) <- "latin1"
        x <- enc2utf8(x)
      }
      if(stripNewline){
        x <- gsub("[\r\n]", " ", x, fixed = FALSE)
      }
      x <- gsub("\t", " ", x, fixed = TRUE)
    }
    return(x)
  })
  return(df)
}


#' Convert Period to GTFS timestamps
#'
#'
#' @param x peridos
#' @noRd
#'
period2gtfs <- function(x) {

  # Check for days
  dys <- lubridate::day(x)
  if(sum(dys, na.rm = TRUE) > 0){
    stop("Days detected in period objects, incorectly formatted period object")
  }

  hrs <- as.character(lubridate::hour(x))
  min <- as.character(lubridate::minute(x))
  sec <- as.character(lubridate::second(x))

  hrs <- ifelse(nchar(hrs) == 1,paste0("0",hrs), hrs)
  min <- ifelse(nchar(min) == 1,paste0("0",min), min)
  sec <- ifelse(nchar(sec) == 1,paste0("0",sec), sec)

  return(paste0(hrs,":",min,":",sec))
}

