
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UK2GTFS

The goal of UK2GTFS is to convert train, tram, bus, and metro timetable
data from the unfriendly and difficult to use formats used in the UK to
the easy to use [GTFS](https://developers.google.com/transit/gtfs/)
format. For a full explanation see the [getting started
vignette](https://itsleeds.github.io/UK2GTFS/articles/articles/transxchange.html).

## Installation

Install the package with **devtools** as
follows:

``` r
install.packages("devtools") # If you do not already have the devtools package
devtools::install_github("ITSleeds/UK2GTFS")
```

## Package Status

This package a work in progress and comes with no guarantees. As of
September 2019, it can convert most train and bus timetables to GTFS but
occasionally fails on specific files.
