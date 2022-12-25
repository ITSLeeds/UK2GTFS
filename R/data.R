#' Agency.txt for ATOC data
#'
#' The ATOC data does not included sufficient information to build agency.txt
#' So this data is provided in the package.
#'
#' @format A data frame
"atoc_agency"

#' Tiploc Locations
#'
#' The ATOC data has inaccurate locations for many tiplocs, this is an improved dataset
#'
#' @format A SF data frame for points
"tiplocs"

#' Activity Codes
#'
#' ATOC activity codes matched to GTFS pickup_type and drop_off_type values.
#'
#' Within the CIF files an Activity code identifies what happens at each
#' location. A full set of activity codes is listed at
#' https://wiki.openraildata.com/index.php?title=Activity_codes. It is possible
#' to have multiple activities happening at the same location. This file
#' contains a look up table to match groups of activity code with their
#' corresponding form in the GTFS file.
#'
#'
#' @format A data frame code 3 columns
"activity_codes"

#' Bus Stop Locations missing from NapTAN
#'
#' A database of bus stops that are missing from the NAPTAN but are known to
#' have been used. For some reason the offical NAPTAN file is missing a small
#' number of bus stops. This file contains a selection of bus stops that have
#' appears in TransXchange files, but are missing in the NAPTAN. The have been
#' assembled from a range of sources and may be of varying quality.
#'
#' In some cases the name of the Bus Stop has been ideitifed but not the location.
#'
#' @format A data frame of 5 columns
"naptan_missing"

#' Bus Stop Locations wrong in the NaPTAN
#'
#' A database of bus stops that are wrong in the NAPTAN. These have been
#' corrected with a mix of manual and automatic techniques. Contributions are
#' welcome of improved locations.
#'
#'
#' @format A data frame of 5 columns
"naptan_replace"


#' Rail Network
#'
#' A simplified version of the UK rail network.
#'
#'
#' @format A SF data frame
"rail"

#' Heavy Rail Network
#'
#' A simplified version of the UK heavy rail network.
#'
#'
#' @format A SF data frame
"rail_heavy"


#' Historic Bank Holidays
#'
#' Bank holidays from 2001 to 2018 in the UK. Note Wales has the same holidays
#' as England.
#'
#'
#' @format A data frame with 4 columns
"historic_bank_holidays"
