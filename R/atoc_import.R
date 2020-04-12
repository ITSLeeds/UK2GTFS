#' Import the .alf file
#'
#' @details
#' Imports the .alf file and returns data.frame
#'
#' @param file Path to .alf file
#' @noRd
#'
importALF <- function(file) {
  nc <- max(utils::count.fields(file, sep = ","))
  table <- utils::read.table(
    file = file,
    header = FALSE,
    sep = ",",
    fill = TRUE,
    col.names = c("M", "O", "D", "T", "S", "E", "P", "F", "U", "R"),
    stringsAsFactors = FALSE
  )

  # Now Fix Misaigned Values
  # Check each column for misalignments
  checkCol <- function(x, val) {
    checkCol.inner <- function(x, val) {
      substr(x, 1, 2) == paste0(val, "=")
    }
    res <- sapply(x, checkCol.inner, val = val)
    return(res)
  }

  for (i in seq(from = 1, to = ncol(table))) {
    colcheck <- checkCol(x = table[[i]], val = names(table)[i])
    if (all(colcheck)) {
      # Simple case remove the "A="
      table[[i]] <- substr(table[[i]], 3, nchar(table[[i]]))
    } else {
      moves <- ifelse(colcheck, NA, table[[i]])
      keeps <- ifelse(colcheck, substr(table[[i]], 3, nchar(table[[i]])), NA)
      attributes(moves) <- NULL
      attributes(keeps) <- NULL

      table[[i]] <- keeps

      # now loop over the moves and put in their place
      for (j in seq(from = 1, to = length(moves))) {
        if (!is.na(moves[j]) & !moves[j] == "") {
          # Find where it should go
          coltogo <- substr(moves[j], 1, 1)
          if (table[j, coltogo] == "") {
            table[j, coltogo] <- moves[j]
          } else {
            message(paste0("Oh now!", i, " ", j))
            stop()
          }
          rm(coltogo)
        }
      }
      rm(moves, keeps)
    }
  }
  return(table)
}


#' Import the .flf file
#'
#' @details
#' Imports the .flf file and returns data.frame
#'
#' @param file Path to .flf file
#' @noRd
#'
importFLF <- function(file) {
  table <- utils::read.table(
    file = file,
    sep = "",
    stringsAsFactors = FALSE,
    header = FALSE,
    fill = TRUE
  )

  # remove end line
  if (table[nrow(table), 1] == "END") {
    table <- table[seq(from = 1, to = (nrow(table) - 1)), ]
  }

  # Bind dother a few columns
  table[[1]] <- paste0(table[[1]], " ", table[[2]])
  names(table) <- c(
    "Type", "del1", "Mode", "del2", "from", "del3",
    "to", "del4", "time", "units"
  )
  table <- table[, c("Type", "Mode", "from", "to", "time", "units")]
  return(table)
}

#' Import the .tsi file
#'
#' @details
#' Imports the .tsi file and returns data.frame
#'
#' @param file Path to .tsi file
#' @noRd
#'
importTSI <- function(file) {
  table <- utils::read.table(
    file = file,
    sep = ",",
    stringsAsFactors = FALSE,
    header = FALSE,
    fill = TRUE
  )

  return(table)
}

#' Import the .msm file
#'
#' @details
#' Imports the .msn file and returns data.frame
#'
#' @param file Path to .flf file
#' @param silent logical, should messages be displayed
#' @noRd
#'
importMSN <- function(file, silent = TRUE) {
  raw <- readLines(
    con = file,
    n = -1
  )
  types <- substr(raw, 1, 1)

  # Physical Station
  if (!silent) {
    message(paste0(Sys.time(), " importing Physical Station"))
  }
  station <- raw[types == "A"]
  station <- station[seq(from = 2, to = length(station))]


  station <- iotools::dstrfw(
    x = station,
    col_types = rep("character", 17 - 1),
    widths = c(1, 4, 26 + 4, 1, 7, 3, 3, 3, 5, 1, 5, 2, 1, 1, 11, 3)
  )

  names(station) <- c(
    "Record Type", "Reserved1", "Station Name",
    "CATE Interchange status", "TIPLOC Code", "CRS Reference Code",
    "Reserved3", "CRS Code", "Ordnance Survey Grid Ref East",
    "Estimate", "Ordnance Survey Grid Ref North", "Minimum Change Time",
    "Reserved4", "Footnote", "Reserved5", "Sub-sector code"
  )

  station$Reserved1 <- NULL
  # n.b. merging Reserved2 into station name as that is what is seems to
  # be for
  station$Reserved3 <- NULL
  station$Reserved4 <- NULL
  station$Reserved5 <- NULL
  station$`Record Type` <- NULL

  station <- strip_whitespace(station)

  # convert to SF object
  # for some reasonf the coordinates are mangled
  station$`Ordnance Survey Grid Ref East` <- as.numeric(station$`Ordnance Survey Grid Ref East`)
  station$`Ordnance Survey Grid Ref North` <- as.numeric(station$`Ordnance Survey Grid Ref North`)
  station$`Ordnance Survey Grid Ref East` <- station$`Ordnance Survey Grid Ref East` * 100 - 1e6
  station$`Ordnance Survey Grid Ref North` <- station$`Ordnance Survey Grid Ref North` * 100 - 6e6

  station <- sf::st_as_sf(station,
    coords = c(
      "Ordnance Survey Grid Ref East",
      "Ordnance Survey Grid Ref North"
    ),
    crs = 27700
  )
  station <- sf::st_transform(station, 4326)

  # GB Timetable numbers
  if (!silent) {
    message(paste0(Sys.time(), " importing GB Timetable numbers"))
  }
  timetable <- raw[types == "B"]
  # timetable = timetable[seq(from = 2, to = length(station))]


  timetable <- iotools::dstrfw(
    x = timetable,
    col_types = rep("character", 5 - 1),
    widths = c(1, 4, 26 + 4, 45)
  )

  names(timetable) <- c(
    "Record Type", "Reserved1", "Station Name",
    "GBTT numbers"
  )

  timetable$Reserved1 <- NULL
  # n.b. merging Reserved2 into station name as that is what is seems
  # to be for
  timetable$`Record Type` <- NULL

  timetable <- strip_whitespace(timetable)

  # Comment Record
  if (!silent) {
    message(paste0(Sys.time(), " importing Comment"))
  }
  comment <- raw[types == "C"]
  comment <- iotools::dstrfw(
    x = comment,
    col_types = rep("character", 2),
    widths = c(1, 79)
  )

  names(comment) <- c("Record Type", "Comment")

  comment$`Record Type` <- NULL
  comment <- strip_whitespace(comment)

  # Alias
  if (!silent) {
    message(paste0(Sys.time(), " importing Alias"))
  }
  alias <- raw[types == "L"]
  alias <- iotools::dstrfw(
    x = alias,
    col_types = rep("character", 6 - 1),
    widths = c(1, 4, 26 + 5, 26, 20)
  )

  names(alias) <- c(
    "Record Type", "Reserved1", "Station Name",
    "Station Alias", "Reserved3"
  )

  alias$Reserved1 <- NULL
  # n.b. merging Reserved2 into station name as that is what is seems
  # to be for
  alias$Reserved3 <- NULL
  alias$`Record Type` <- NULL
  alias <- strip_whitespace(alias)

  result <- list(station, timetable, comment, alias)
  names(result) <- c("station", "timetable", "comment", "alias")
  return(result)
}

#' Strip White Space
#'
#' @details
#' Strips whitespace from a dataframe of charters vectors and returns
#'     the data frame
#'
#' @param df data frame
#' @noRd
#'
strip_whitespace <- function(df) {
  sws <- function(val) {
    val <- trimws(val, which = "right")
    val[val == ""] <- NA
    return(val)
  }
  df[] <- lapply(df, sws)
  return(df)
}

#' Import the .mca file
#'
#' @details
#' Imports the .mca file and returns data.frame
#'
#' @param file Path to .mca file
#' @param silent logical, should messages be displayed
#' @param ncores number of cores to use when paralell processing
#' @param full_import import all data, default FALSE
#' @noRd
#'
importMCA <- function(file,
                      silent = TRUE,
                      ncores = 1,
                      full_import = FALSE) {

  # see https://wiki.openraildata.com/index.php/CIF_File_Format
  if (!silent) {
    message(paste0(Sys.time(), " reading .mca file"))
  }
  raw <- readLines(
    con = file,
    n = -1
  )
  types <- substr(raw, 1, 2)

  # break out each part of the file
  # Header Record
  # Not Needed

  # Basic Schedule
  if (!silent) {
    message(paste0(Sys.time(), " importing Basic Schedule"))
  }
  BS <- raw[types == "BS"]
  BS <- iotools::dstrfw(
    x = BS,
    col_types = rep("character", 26),
    widths = c(
      2, 1, 6, 6, 6, 7, 1, 1, 2, 4, 4, 1, 8, 1, 3, 4, 3,
      6, 1, 1, 1, 1, 4, 4, 1, 1
    )
  )
  names(BS) <- c(
    "Record Identity", "Transaction Type", "Train UID", "Date Runs From",
    "Date Runs To", "Days Run", "Bank Holiday Running", "Train Status",
    "Train Category", "Train Identity", "Headcode", "Course Indicator",
    "Profit Centre Code/ Train Service Code", "Business Sector",
    "Power Type", "Timing Load", "Speed", "Operating Chars",
    "Train Class", "Sleepers", "Reservations", "Connect Indicator",
    "Catering Code", "Service Branding", "Spare", "STP indicator"
  )

  BS$Spare <- NULL
  BS$`Record Identity` <- NULL
  BS <- strip_whitespace(BS)

  # clean data
  BS$`Date Runs From` <- as.Date(BS$`Date Runs From`, format = "%y%m%d")
  BS$`Date Runs To` <- as.Date(BS$`Date Runs To`, format = "%y%m%d")

  BS$`Transaction Type` <- as.factor(BS$`Transaction Type`)
  BS$`Train Status` <- as.factor(BS$`Train Status`)
  BS$`Train Category` <- as.factor(BS$`Train Category`)
  BS$`Power Type` <- as.factor(BS$`Power Type`)
  BS$`STP indicator` <- as.factor(BS$`STP indicator`)

  BS$Speed <- as.integer(BS$Speed)

  # Add the rowid
  BS$rowID <- seq(from = 1, to = length(types))[types == "BS"]

  # Basic Schedule Extra Details
  if (!silent) {
    message(paste0(Sys.time(), " importing Basic Schedule Extra Details"))
  }
  BX <- raw[types == "BX"]
  BX <- iotools::dstrfw(
    x = BX,
    col_types = rep("character", 8),
    widths = c(2, 4, 5, 2, 1, 8, 1, 57)
  )
  names(BX) <- c(
    "Record Identity", "Traction Class", "UIC Code", "ATOC Code",
    "Applicable Timetable Code", "Retail Train ID", "Source", "Spare"
  )
  BX$Spare <- NULL
  BX$`Record Identity` <- NULL
  BX <- strip_whitespace(BX)
  # clean data

  # Add the rowid
  BX$rowID <- seq(from = 1, to = length(types))[types == "BX"]

  # Origin Station
  if (!silent) {
    message(paste0(Sys.time(), " importing Origin Station"))
  }
  LO <- raw[types == "LO"]
  LO <- iotools::dstrfw(
    x = LO,
    col_types = rep("character", 12),
    widths = c(2, 7, 1, 5, 4, 3, 3, 2, 2, 12, 2, 37)
  )
  names(LO) <- c(
    "Record Identity", "Location", "Suffix", "Scheduled Departure Time",
    "Public Departure Time", "Platform", "Line", "Engineering Allowance",
    "Pathing Allowance", "Pathing Allowance", "Performance Allowance",
    "Spare"
  )
  LO$Spare <- NULL
  LO$`Record Identity` <- NULL
  LO <- strip_whitespace(LO)
  LO$`Scheduled Departure Time` <- gsub("H", "",
                                        LO$`Scheduled Departure Time`)

  LO <- LO[, c("Location", "Scheduled Departure Time")]

  # Add the rowid
  LO$rowID <- seq(from = 1, to = length(types))[types == "LO"]

  # Intermediate Station
  if (!silent) {
    message(paste0(Sys.time(), " importing Intermediate Station"))
  }
  LI <- raw[types == "LI"]
  LI <- iotools::dstrfw(
    x = LI,
    col_types = rep("character", 16),
    widths = c(2, 7, 1, 5, 5, 5, 4, 4, 3, 3, 3, 12, 2, 2, 2, 20)
  )
  names(LI) <- c(
    "Record Identity", "Location", "Suffix", "Scheduled Arrival Time",
    "Scheduled Departure Time", "Scheduled Pass", "Public Arrival Time",
    "Public Departure Time", "Platform", "Line", "Path", "Activity",
    "Engineering Allowance", "Pathing Allowance", "Performance Allowance",
    "Spare"
  )
  LI$Spare <- NULL
  LI$`Record Identity` <- NULL
  LI <- strip_whitespace(LI)

  # Add the rowid
  LI$rowID <- seq(from = 1, to = length(types))[types == "LI"]

  # Filter to stops for passengers
  acts <- c(
    "T", # Stops to take up and set down passengers
    "D", # Stops to set down passengers
    "U" # Stops to take up passengers
  )

  LI <- LI[sapply(strsplit(LI$Activity, " "), function(x) {
    any(acts %in% x)
  }), ]
  # Check for errors in the times
  LI$`Scheduled Arrival Time` <- gsub("H", "",
                                      LI$`Scheduled Arrival Time`)
  LI$`Scheduled Departure Time` <- gsub("H", "",
                                        LI$`Scheduled Departure Time`)

  LI <- LI[, c(
    "Location", "Scheduled Arrival Time",
    "Scheduled Departure Time", "Activity", "rowID"
  )]



  # Terminating Station
  if (!silent) {
    message(paste0(Sys.time(), " importing Terminating Station"))
  }
  LT <- raw[types == "LT"]
  LT <- iotools::dstrfw(
    x = LT,
    col_types = rep("character", 9),
    widths = c(2, 7, 1, 5, 4, 3, 3, 12, 43)
  )
  names(LT) <- c(
    "Record Identity", "Location", "Suffix", "Scheduled Arrival Time",
    "Public Arrival Time", "Platform", "Path", "Activity", "Spare"
  )
  LT$Spare <- NULL
  LT$`Record Identity` <- NULL
  LT <- strip_whitespace(LT)
  LT$`Scheduled Arrival Time` <- gsub("H", "", LT$`Scheduled Arrival Time`)

  LT <- LT[, c("Location", "Scheduled Arrival Time", "Activity")]

  # Add the rowid
  LT$rowID <- seq(from = 1, to = length(types))[types == "LT"]

  # TIPLOC Insert
  if (full_import) {
    # Changes En Route
    if (!silent) {
      message(paste0(Sys.time(), " importing Changes En Route"))
    }
    CR <- raw[types == "CR"]
    CR <- iotools::dstrfw(
      x = CR,
      col_types = rep("character", 22),
      widths = c(
        2, 8, 2, 4, 4, 1, 8, 1, 3, 4, 3, 6, 1, 1, 1, 1, 4,
        4, 4, 5, 8, 5
      )
    )
    names(CR) <- c(
      "Record Identity", "Location", "Train Category", "Train Identity",
      "Headcode", "Course Indicator",
      "Profit Centre Code/ Train Service Code",
      "Business Sector", "Power Type", "Timing Load", "Speed",
      "Operating Chars", "Train Class", "Sleepers", "Reservations",
      "Connect Indicator", "Catering Code", "Service Branding",
      "Traction Class", "UIC Code", "Retail Train ID", "Spare"
    )
    CR$Spare <- NULL
    CR$`Record Identity` <- NULL
    CR <- strip_whitespace(CR)

    # Add the rowid
    CR$rowID <- seq(from = 1, to = length(types))[types == "CR"]

    if (!silent) {
      message(paste0(Sys.time(), " importing TIPLOC Insert"))
    }
    TI <- raw[types == "TI"]
    TI <- iotools::dstrfw(
      x = TI,
      col_types = rep("character", 11),
      widths = c(2, 7, 2, 6, 1, 26, 5, 4, 3, 16, 8)
    )
    names(TI) <- c(
      "Record Identity", "TIPLOC code", "Capitals", "NALCO",
      "NLC Check Character", "TPS Description",
      "STANOX", "PO MCP Code", "CRS Code", "Description", "Spare"
    )
    TI$Spare <- NULL
    TI$`Record Identity` <- NULL
    TI <- strip_whitespace(TI)

    # Add the rowid
    TI$rowID <- seq(from = 1, to = length(types))[types == "TI"]

    # TIPLOC Amend
    if (!silent) {
      message(paste0(Sys.time(), " importing TIPLOC Amend"))
    }
    TA <- raw[types == "TA"]
    TA <- iotools::dstrfw(
      x = TA,
      col_types = rep("character", 12),
      widths = c(2, 7, 2, 6, 1, 26, 5, 4, 3, 16, 7, 1)
    )
    names(TA) <- c(
      "Record Identity", "TIPLOC code", "Capitals", "NALCO",
      "NLC Check Character", "TPS Description", "STANOX", "PO MCP Code",
      "CRS Code", "Description", "New TIPLOC", "Spare"
    )
    TA$Spare <- NULL
    TA$`Record Identity` <- NULL
    TA <- strip_whitespace(TA)

    # Add the rowid
    TA$rowID <- seq(from = 1, to = length(types))[types == "TA"]

    # TIPLOC Delete
    if (!silent) {
      message(paste0(Sys.time(), " importing TIPLOC Delete"))
    }
    TD <- raw[types == "TA"]
    TD <- iotools::dstrfw(
      x = TD,
      col_types = rep("character", 3),
      widths = c(2, 7, 71)
    )
    names(TD) <- c("Record Identity", "TIPLOC code", "Spare")
    TD$Spare <- NULL
    TD$`Record Identity` <- NULL
    TD <- strip_whitespace(TD)

    # Add the rowid
    TD$rowID <- seq(from = 1, to = length(types))[types == "TD"]
  }


  # Association
  if (full_import) {
    if (!silent) {
      message(paste0(Sys.time(), " importing Association"))
    }
    AA <- raw[types == "AA"]
    AA <- iotools::dstrfw(
      x = AA,
      col_types = rep("character", 16),
      widths = c(2, 1, 6, 6, 6, 6, 7, 2, 1, 7, 1, 1, 1, 1, 31, 1)
    )
    names(AA) <- c(
      "Record Identity", "Transaction Type", "Base UID", "Assoc UID",
      "Assoc Start date", "Assoc End date", "Assoc Days", "Assoc Cat",
      "Assoc Date Ind", "Assoc Location", "Base Location Suffix",
      "Assoc Location Suffix", "Diagram Type", "Association Type",
      "Filler", "STP indicator"
    )

    AA$Filler <- NULL
    AA$`Record Identity` <- NULL
    AA <- strip_whitespace(AA)

    AA$`Assoc Start date` <- as.Date(AA$`Assoc Start date`, format = "%d%m%y")
    AA$`Assoc End date` <- as.Date(AA$`Assoc End date`, format = "%d%m%y")


    AA$`Transaction Type` <- as.factor(AA$`Transaction Type`)
    AA$`Assoc Date Ind` <- as.factor(AA$`Assoc Date Ind`)
    AA$`Diagram Type` <- as.factor(AA$`Diagram Type`)
    AA$`Association Type` <- as.factor(AA$`Association Type`)
    AA$`STP indicator` <- as.factor(AA$`STP indicator`)

    AA$`Assoc Location Suffix` <- as.integer(AA$`Assoc Location Suffix`)

    # Add the rowid
    AA$rowID <- seq(from = 1, to = length(types))[types == "AA"]
  }

  # Trailer Record
  if (!silent) {
    message(paste0(Sys.time(), " importing Trailer Record"))
  }
  ZZ <- raw[types == "ZZ"]
  ZZ <- iotools::dstrfw(
    x = ZZ,
    col_types = rep("character", 2),
    widths = c(2, 78)
  )
  names(ZZ) <- c("Record Identity", "Spare")
  ZZ$Spare <- NULL
  ZZ <- strip_whitespace(ZZ)

  # Add the rowid
  ZZ$rowID <- seq(from = 1, to = length(types))[types == "ZZ"]

  # Prep the main files
  if (!silent) {
    message(paste0(Sys.time(), " Preparing Imported Data"))
  }
  stop_times <- dplyr::bind_rows(list(LO, LI, LT))
  stop_times <- stop_times[order(stop_times$rowID), ]
  stop_times$schedule <- as.integer(as.character(cut(stop_times$rowID,
    c(BS$rowID, ZZ$rowID[1]),
    labels = BS$rowID
  )))
  stop_times$stop_sequence <- sequence(rle(stop_times$schedule)$lengths)


  BX$rowIDm1 <- BX$rowID - 1
  BX$rowID <- NULL
  schedule <- dplyr::left_join(BS, BX, by = c("rowID" = "rowIDm1"))

  if (full_import) {
    results <- list(stop_times, schedule, TI, TA, TD, AA, CR)
    names(results) <- c("stop_times", "schedule", "TI", "TA", "TD", "AA", "CR")
  } else {
    results <- list(stop_times, schedule)
    names(results) <- c("stop_times", "schedule")
  }

  return(results)
}
