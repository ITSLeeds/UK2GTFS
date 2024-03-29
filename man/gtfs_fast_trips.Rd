% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs_cleaning.R
\name{gtfs_fast_trips}
\alias{gtfs_fast_trips}
\title{Find fast trips}
\usage{
gtfs_fast_trips(gtfs, maxspeed = 83, routes = TRUE)
}
\arguments{
\item{gtfs}{list of gtfs tables}

\item{maxspeed}{the maximum allowed speed in metres per second default 83 m/s
(about 185 mph the max speed of trains on HS1 line)}

\item{routes}{logical, do one trip per route, faster but may miss some trips}
}
\description{
Fast trips can identify problems with the input data or
  conversion process. This function returns trip_ids for trips that exceed
  `maxspeed`.
}
\details{
The function looks a straight line distance between each stop and
  detects the fastest segment of the journey. A common cause of errors is
  that a stop is in the wrong location so a bus can appear to teleport
  across the country in seconds.
}
