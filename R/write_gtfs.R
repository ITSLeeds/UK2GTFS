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
#' @param quote logical, should strings be quoted, default = FALSE, passed to data.table::fwrite
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

  if("frequencies" %in% names(gtfs))
  {
    if("difftime" %in% class(gtfs$frequencies$start_time)){
      gtfs$frequencies$start_time <- format(gtfs$frequencies$start_time, format = "%H:%M:%S")
    }

    if("difftime" %in% class(gtfs$frequencies$end_time)){
      gtfs$frequencies$end_time <- format(gtfs$frequencies$end_time, format = "%H:%M:%S")
    }
  }

  dir.create(paste0(tempdir(), "/gtfs_temp"))

  for ( tableName in names(gtfs) )
  {
    table <- gtfs[[tableName]]

    if ( !is.null(table) & nrow(table) > 0 )
    {
      data.table::fwrite(table, file.path(tempdir(), "gtfs_temp", paste0(tableName, ".txt")), row.names = FALSE, quote = quote)
    }
  }

  zip::zipr(paste0(folder, "/", name, ".zip"), list.files(paste0(tempdir(), "/gtfs_temp"), full.names = TRUE), recurse = FALSE)

  unlink(paste0(tempdir(), "/gtfs_temp"), recursive = TRUE)
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
#' When writing a 400mb (zipped) file, we spend nearly 4 minutes in this fn(), about 10x longer than writing the files to the filesystem.
#' profiler reports this being mostly nchar(), so we optimise down to one sprintf which reduces the time to 1 minute
#' .format() is about 7x slower than sprintf()
#'
#' @param x periods
#' @noRd
#'
period2gtfs <- function(x) {

  # Check for days
  dys <- lubridate::day(x)
  if(sum(dys, na.rm = TRUE) > 0){
    stop("Days detected in period objects, incorectly formatted period object")
  }

  return( sprintf("%02d:%02d:%02d", lubridate::hour(x), lubridate::minute(x), lubridate::second(x)) )
}

