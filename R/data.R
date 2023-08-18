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

#' Example School term and holiday dates
#'
#' Term dates change around the UK and from year to year. This are simply the
#' Leeds 2022/23 dates repeated for each year. Therefore some inaccuracy will
#' occur when used historically
#'
#'
#' @format A data frame with 4 columns
"school_terms"


