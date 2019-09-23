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
#' ATOC activity codes matche to GTFS pickup and drop off values
#'
#'
#' @format A data frame code 3 columns
"activity_codes"

#' Bus Stop Locations
#'
#' A database of bus stops that are missing from the NAPTAN but are known to have been used.
#'
#'
#' @format A data frame of 5 columns
"naptan_missing"
