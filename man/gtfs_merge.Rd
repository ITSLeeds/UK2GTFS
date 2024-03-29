% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gtfs_merge.R
\name{gtfs_merge}
\alias{gtfs_merge}
\title{merge a list of gtfs files}
\usage{
gtfs_merge(gtfs_list, force = FALSE, quiet = TRUE)
}
\arguments{
\item{gtfs_list}{a list of gtfs objects to be merged}

\item{force}{logical, if TRUE duplicated values are merged taking the fist}

\item{quiet}{logical, if TRUE less messages
instance to be the correct instance, in most cases this is ok, but may
cause some errors}
}
\description{
merge a list of gtfs files
}
