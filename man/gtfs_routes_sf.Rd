% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs_to_sf.R
\name{gtfs_routes_sf}
\alias{gtfs_routes_sf}
\title{Make routes into an SF object}
\usage{
gtfs_routes_sf(gtfs)
}
\arguments{
\item{gtfs}{a gtfs object}
}
\value{
sf data frame of lines
}
\description{
Make similar to `gtfs_trips_sf` expect one trip is chosen to represent each route
}
