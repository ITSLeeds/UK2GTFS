% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transxchange.R
\name{transxchange2gtfs}
\alias{transxchange2gtfs}
\title{TransXchange to GTFS}
\usage{
transxchange2gtfs(
  path_in,
  silent = TRUE,
  ncores = 1,
  cal = get_bank_holidays(),
  naptan = get_naptan(),
  scotland = "auto",
  try_mode = TRUE,
  force_merge = FALSE
)
}
\arguments{
\item{path_in}{Path to zipped transxchange files}

\item{silent}{Logical, should progress be shown}

\item{ncores}{Numeric, When parallel processing how many cores to use}

\item{cal}{Calendar object from get_bank_holidays()}

\item{naptan}{Naptan stop locations from get_naptan()}

\item{scotland}{character, should Scottish bank holidays be used? Can be
"auto" (default), "yes", "no". If "auto" and path_in ends with "S.zip"
Scottish bank holidays will be used, otherwise England and Wales bank
holidays are used.}

\item{try_mode}{Logical, if TRUE import and conversion are wrapped in try
calls thus a failure on a single file will not cause the whole process to
fail. Warning this could result in a GTFS file with missing routes.}

\item{force_merge}{Logical, passed to gtfs_merge(force), default FALSE}
}
\value{
A GTFS named list
}
\description{
TransXchange to GTFS
}
\details{
Convert transxchange files to GTFS

This is a meta function which aids TransXchange to GTFS conversion. It simple
runs transxchange_import(), transxchange_export(), gtfs_merge(), gtfs_write()

Progress Bars

To minimise overall processing when using multiple cores the function works
from largest to smallest file.This can mean the progress bar sits a 0% for
quite some time, before starting to move rapidly.
}
