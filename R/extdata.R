#' Update the data inside the UK2GTFS package
#'
#' As UK2GTFS has large datasets that update separately to the R package they
#' are checked and downloaded at package load time. This function checks for and
#' downloaded any updated to the data.
#'
#' Raw data can be viewed and contributed to at
#' https://github.com/ITSLeeds/UK2GTFS-data
#'
#' @export
#'
update_data <- function( timeout=60 ){

  check <- check_data( timeout=timeout )

  if(check$date_package != check$date){

    if (interactive()) {

      response <- readline("UK2GTFS data is out of date. Do you want to update? (yes/no): ")
      response <- tolower(response)

      if (response == "yes" || response == "y") {
        message("Updating internal package data")
        download_data(check$tag_name, check$package_location, check$date)
      } else if (response == "no" || response == "n") {
        cat("You can rerun this check with update_data()")
      } else {
        cat("Invalid response.\n")
      }


    } else {
      message("Data not updated, run update_data()")
      message("Updating internal package data")
      download_data(check$tag_name, check$package_location, check$date)
    }

  } else {
    message("Your UK2GTFS data is up to date")
  }

}

#' Update the data inside the UK2GTFS package
#'
#' As UK2GTFS has large datasets that update separately to the R package they
#' are checked and downloaded a package load time
#'
#' @noRd
#'
download_data <- function(tag_name, package_location, date){

  dir.create(file.path(tempdir(),"UK2GTFS_load"))
  utils::download.file(paste0("https://github.com/ITSLeeds/UK2GTFS-data/releases/download/",
                       tag_name,"/all.zip"),
                destfile = file.path(tempdir(),"UK2GTFS_load/all.zip"),
                mode = "wb")
  utils::unzip(file.path(tempdir(),"UK2GTFS_load/all.zip"),
               exdir = file.path(package_location, "extdata"))
  unlink(file.path(tempdir(),"UK2GTFS_load"), recursive = TRUE)
  writeLines( as.character(date), file.path(package_location, "extdata/date.txt"))

}


#' Check if data in package is up to date
#'
#' As UK2GTFS has large datasets that update separately to the R package they
#' are checked and downloaded a package load time
#' @param default_tag What release to assume if unable to check.
#' @return TRUE if data is up-to-date or if unable to check
#' @noRd

check_data <- function( timeout = 60, default_tag = "v0.1.2"){
  # Try not to hammer the API
  Sys.sleep(5)
  # Check date on data repo
  res = try(httr::GET("https://api.github.com/repos/ITSleeds/UK2GTFS-data/releases", httr::timeout(get("timeout")),
            silent = TRUE ))
  if(inherits(res, "try-error")){
    message("Unable to check for latest data")
    date = Sys.time()
    tag_name = default_tag
  } else {
    res = RcppSimdJson::fparse(res$content)
    date = res$published_at[1]
    if(is.null(date)){
      message("Unable to check for latest data")
      date = Sys.time()
      tag_name = default_tag
    } else {
      tag_name = res$tag_name[1]
    }
  }

  date = as.Date(date) #make it less sensitive by only comparing date rather than date+time

  #Check if date.txt in package
  package_location <- system.file(package = "UK2GTFS")
  if(!file.exists(file.path(package_location, "extdata/date.txt"))){
    writeLines("nodata", file.path(package_location, "extdata/date.txt"))
  }

  tryCatch({
    date_package <- as.Date( readLines(file.path(package_location, "extdata/date.txt")) )
  }, error = function(err) {
    date_package = "nodata"
  })


  return(list(date_package = date_package, date = date, tag_name = tag_name,
              package_location = package_location))
}

#' Load a built-in UK2GTFS dataset
#'
#' As UK2GTFS has large datasets that update separately to the R package they
#' are checked and downloaded at package load time.
#'
#' This function loads a dataset. Examples are:
#'
#'
#' "atco_areas" ATCO Admin Areas
#'
#' Association of Transport Coordinating Officers
#'
#' Boundaries of the ATCO Admin Areas. Note there are 4 national areas
#' represented by a box around the UK.
#'
#' "atoc_agency" Agency.txt for ATOC data
#'
#' The ATOC data does not included sufficient information to build agency.txt
#' So this data is provided in the package.
#'
#' "tiplocs" Tiploc Locations
#'
#' The ATOC data has inaccurate locations for many tiplocs, this is an improved dataset
#'
#' "naptan_missing" Bus Stop Locations missing from NapTAN
#'
#' A database of bus stops that are missing from the NAPTAN but are known to
#' have been used. For some reason the official NAPTAN file is missing a small
#' number of bus stops. This file contains a selection of bus stops that have
#' appears in TransXchange files, but are missing in the NAPTAN. The have been
#' assembled from a range of sources and may be of varying quality.
#'
#' In some cases the name of the Bus Stop has been identified but not the location.
#'
#' "naptan_replace" Bus Stop Locations wrong in the NaPTAN
#'
#' A database of bus stops that are wrong in the NAPTAN. These have been
#' corrected with a mix of manual and automatic techniques. Contributions are
#' welcome of improved locations.
#'
#' "rail_light" Light Rail Network
#'
#' A simplified version of the UK light rail network.
#'
#' "rail_heavy" Heavy Rail Network
#'
#' A simplified version of the UK heavy rail network.
#'
#' "historic_bank_holidays" Historic Bank Holidays
#'
#' Bank holidays from 2001 to 2018 in the UK. Note Wales has the same holidays
#' as England.
#'
#'
#' @param type name of data to be loaded e.g. "tiplocs"
#' @return Loads named object into the global environment
#' @export

load_data <- function(type){

  package_location <- system.file(package = "UK2GTFS")
  load(file.path(package_location, "extdata", paste0(type,".rda")),
       envir = globalenv())

}

