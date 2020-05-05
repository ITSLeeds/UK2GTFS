#' Agency.txt for ATOC data
#'
#' The ATOC data does not included sufficent infomration to build agency.txt
#' So this data is provided in the package.
#'
#' @format A data frame
"atoc_agency"

#' Tiploc Locations
#'
#' The ATOC data has innacuate locations for many tiplocs, this is an improved dataset
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
#' to have multiple acticities happening at the same location. This file
#' contains a lookup table to match groups of acticity code with their
#' corresponding form in the GTFS file.
#'
#'
#' @format A data frame code 3 columns
"activity_codes"

#' Bus Stop Locations
#'
#' A database of bus stops that are missing from the NAPTAN but are known to
#' have been used. For some reason the offical NAPTAN file is missing a small
#' number of bus stops. This file contains a selection of bus stops that have
#' appears in TransXchange files, but are missing in the NAPTAN. The have been
#' assempled from a rnage of sources and may be of varying quality.
#'
#' @format A data frame of 5 columns
"naptan_missing"

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
