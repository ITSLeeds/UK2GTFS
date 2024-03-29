% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{activity_codes}
\alias{activity_codes}
\title{Activity Codes}
\format{
A data frame code 3 columns
}
\usage{
activity_codes
}
\description{
ATOC activity codes matched to GTFS pickup_type and drop_off_type values.
}
\details{
Within the CIF files an Activity code identifies what happens at each
location. A full set of activity codes is listed at
https://wiki.openraildata.com/index.php?title=Activity_codes. It is possible
to have multiple activities happening at the same location. This file
contains a look up table to match groups of activity code with their
corresponding form in the GTFS file.
}
\keyword{datasets}
