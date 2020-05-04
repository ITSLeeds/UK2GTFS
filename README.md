
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UK2GTFS <a href='https://itsleeds.github.io/'><img src='man/figures/logo.png' align="right" height=215/></a>

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ITSLeeds/UK2GTFS.svg?branch=master)](https://travis-ci.org/itsleeds/UK2GTFS)
[![CRAN
status](https://www.r-pkg.org/badges/version/UK2GTFS)](https://cran.r-project.org/package=UK2GTFS)
[![codecov](https://codecov.io/gh/itsleeds/UK2GTFS/branch/master/graph/badge.svg)](https://codecov.io/gh/itsleeds/UK2GTFS)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/itsleeds/UK2GTFS?branch=master&svg=true)](https://ci.appveyor.com/project/mem48/UK2GTFS)
<!-- badges: end -->

UK2GTFS is an R package to convert train, tram, bus, and metro timetable
data from the unfriendly and difficult to use formats used in the UK to
the easy to use [GTFS](https://developers.google.com/transit/gtfs/)
format. The main purpose of developing the package is to support using
[OpenTripPlanner](https://github.com/ropensci/opentripplanner) in the
UK.

**Example results are published as [GitHub
releases](https://github.com/ITSLeeds/UK2GTFS/releases) these come with
no guarantee of quality.**

## Introduction

The UK has two main sources of public transport timetable data
[**traveline**](https://www.travelinedata.org.uk/) publishes data on
buses and light rail and
[**ATOC**](http://data.atoc.org/rail-industry-data) publishes data on
heavy rail. Each uses a data format that is unique to that organisation
and both formats are old and difficult to use. Many countries have
adopted the [GTFS](https://developers.google.com/transit/gtfs/) format
which was developed by Google as a simple standard for publishing
timetable data. Due to the wide-spread adoption of GTFS many useful
tools have been developed that accept GTFS files as inputs. This package
is intended to make the conversion of UK data to the GTFS format simple
and easy.

## Capabilities - why we need another package

There are a number of pre-existing options for converting data to GTFS.
This package aims to go beyond basic conversion by providing a range of
additional functionality:

1.  Data conversion
      - Conversion of TransXchange to GTFS
      - Conversion of ATOC CIF files to GTFS
      - reading of TransXchange and CIF files is also supported,
        although some parts which are not required for GTFS conversion
        are only partially supported.
2.  Data cleaning, the raw data often contains clear errors, the package
    does not blindly convert but also corrects some known errors
      - Improved locations of tiplocs - used to locate feature on the
        heavy rail network
      - Add bus stops missing in the NAPTAN
      - Correction of spelling errors
      - Removal of on-demand bus services (GTFS does not support
        services that are on-demand)
3.  Data polishing
      - Support of journeys past midnight
      - Routing of train journeys along tracks rather than straight
        lines between stops (work in progress)
4.  GTFS tools, functions for working with GTFS data
      - Reading / Writing
      - Cleaning
      - Compression
      - Validation
      - Subsetting

## Installation

Install the package with **remotes** as follows:

``` r
install.packages("remotes") # If you do not already have the remotes package
remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)
```

## Use

You can find [full documentation for the
package](https://itsleeds.github.io/UK2GTFS/), including a list of
[functions](https://itsleeds.github.io/UK2GTFS/reference/index.html) and
more detail on converting
[TransXchange](https://itsleeds.github.io/UK2GTFS/articles/transxchange.html)
and [ATOC CIF](https://itsleeds.github.io/UK2GTFS/articles/ATOC.html).

## Package Status

This package a work in progress and comes with no guarantees. As of
September 2019, it can convert most train and bus timetables to GTFS but
occasionally fails on specific files.

Please report failed conversions as GitHub
[Issues](https://github.com/itsleeds/uk2gtfs/issues)
