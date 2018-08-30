# Rethinking the Import process

#' Import the .mca file
#'
#' @details
#' Imports the .mca file and returns data.frame
#'
#' @param file Path to .mca file
#'
importMCA_alt <- function(file, silent = TRUE, ncores = 1){

  # see https://wiki.openraildata.com/index.php/CIF_File_Format
  if(!silent){message(paste0(Sys.time()," reading .mca file"))}
  raw = readLines(con = file,
                  n = -1)
  types = substr(raw, 1, 2)

  #break out each part of the file
  # Header Record
  # Not Needed

  # Basic Schedule
  if(!silent){message(paste0(Sys.time()," importing Basic Schedule"))}
  BS = raw[types == "BS"]
  BS = iotools::dstrfw(x = BS,
                       col_types = rep('character',26),
                       widths = c(2,1,6,6,6,7,1,1,2,4,4,1,8,1,3,4,3,6,1,1,1,1,4,4,1,1))
  names(BS) = c("Record Identity","Transaction Type","Train UID","Date Runs From","Date Runs To",
                "Days Run","Bank Holiday Running","Train Status","Train Category","Train Identity",
                "Headcode","Course Indicator","Profit Centre Code/ Train Service Code","Business Sector",
                "Power Type","Timing Load","Speed","Operating Chars","Train Class","Sleepers","Reservations",
                "Connect Indicator","Catering Code","Service Branding","Spare","STP indicator")

  BS$Spare = NULL
  BS$`Record Identity` = NULL
  BS = strip_whitespace(BS)

  # clean data
  BS$`Date Runs From` = as.Date(BS$`Date Runs From`, format = "%y%m%d")
  BS$`Date Runs To` = as.Date(BS$`Date Runs To`, format = "%y%m%d")

  BS$`Transaction Type` = as.factor(BS$`Transaction Type`)
  BS$`Train Status` = as.factor(BS$`Train Status`)
  BS$`Train Category` = as.factor(BS$`Train Category`)
  BS$`Power Type` = as.factor(BS$`Power Type`)
  BS$`STP indicator` = as.factor(BS$`STP indicator`)

  BS$Speed = as.integer(BS$Speed)

  # Add the rowid
  BS$rowID = seq(from = 1, to = length(types))[types == "BS"]

  # Basic Schedule Extra Details
  if(!silent){message(paste0(Sys.time()," importing Basic Schedule Extra Details"))}
  BX = raw[types == "BX"]
  BX = iotools::dstrfw(x = BX,
                       col_types = rep('character',8),
                       widths = c(2,4,5,2,1,8,1,57))
  names(BX) = c("Record Identity","Traction Class","UIC Code","ATOC Code","Applicable Timetable Code",
                "Retail Train ID", "Source", "Spare")
  BX$Spare = NULL
  BX$`Record Identity` = NULL
  BX = strip_whitespace(BX)
  # clean data

  # Add the rowid
  BX$rowID = seq(from = 1, to = length(types))[types == "BX"]

  # Origin Station
  if(!silent){message(paste0(Sys.time()," importing Origin Station"))}
  LO = raw[types == "LO"]
  LO = iotools::dstrfw(x = LO,
                       col_types = rep('character',12),
                       widths = c(2,7,1,5,4,3,3,2,2,12,2,37))
  names(LO) = c("Record Identity","Location","Suffix","Scheduled Departure Time","Public Departure Time","Platform",
                "Line","Engineering Allowance","Pathing Allowance","Pathing Allowance","Performance Allowance","Spare")
  LO$Spare = NULL
  LO$`Record Identity` = NULL
  LO = strip_whitespace(LO)

  LO = LO[,c("Location","Public Departure Time")]

  # Add the rowid
  LO$rowID = seq(from = 1, to = length(types))[types == "LO"]

  # Intermediate Station
  if(!silent){message(paste0(Sys.time()," importing Intermediate Station"))}
  LI = raw[types == "LI"]
  LI = iotools::dstrfw(x = LI,
                       col_types = rep('character',16),
                       widths = c(2,7,1,5,5,5,4,4,3,3,3,12,2,2,2,20))
  names(LI) = c("Record Identity","Location","Suffix","Scheduled Arrival Time","Scheduled Departure Time",
                "Scheduled Pass", "Public Arrival Time", "Public Departure Time", "Platform", "Line",
                "Path","Activity","Engineering Allowance","Pathing Allowance","Performance Allowance","Spare")
  LI$Spare = NULL
  LI$`Record Identity` = NULL
  LI = strip_whitespace(LI)

  # Add the rowid
  LI$rowID = seq(from = 1, to = length(types))[types == "LI"]

  # Filter to stops for passengers
  acts = c("T", # Stops to take up and set down passengers
           "D", # Stops to set down passengers
           "U" # Stops to take up passengers
           )

  LI = LI[sapply(strsplit(LI$Activity, " "),function(x){any(acts %in% x)}),]
  LI = LI[,c("Location","Public Arrival Time","Public Departure Time","Activity","rowID")]



  # Terminating Station
  if(!silent){message(paste0(Sys.time()," importing Terminating Station"))}
  LT = raw[types == "LT"]
  LT = iotools::dstrfw(x = LT,
                       col_types = rep('character',9),
                       widths = c(2,7,1,5,4,3,3,12,43))
  names(LT) = c("Record Identity","Location","Suffix","Scheduled Arrival Time","Public Arrival Time",
                "Platform","Path","Activity","Spare")
  LT$Spare = NULL
  LT$`Record Identity` = NULL
  LT = strip_whitespace(LT)

  LT = LT[,c("Location","Public Arrival Time","Activity")]

  # Add the rowid
  LT$rowID = seq(from = 1, to = length(types))[types == "LT"]

  # Changes En Route
  if(!silent){message(paste0(Sys.time()," importing Changes En Route"))}
  CR = raw[types == "CR"]
  CR = iotools::dstrfw(x = CR,
                       col_types = rep('character',22),
                       widths = c(2,8,2,4,4,1,8,1,3,4,3,6,1,1,1,1,4,4,4,5,8,5))
  names(CR) = c("Record Identity","Location","Train Category","Train Identity","Headcode","Course Indicator",
                "Profit Centre Code/ Train Service Code","Business Sector","Power Type","Timing Load","Speed",
                "Operating Chars","Train Class","Sleepers","Reservations","Connect Indicator","Catering Code",
                "Service Branding","Traction Class","UIC Code","Retail Train ID","Spare")
  CR$Spare = NULL
  CR$`Record Identity` = NULL
  CR = strip_whitespace(CR)

  # Add the rowid
  CR$rowID = seq(from = 1, to = length(types))[types == "CR"]

  #TIPLOC Insert
  if(!silent){message(paste0(Sys.time()," importing TIPLOC Insert"))}
  TI = raw[types == "TI"]
  TI = iotools::dstrfw(x = TI,
                       col_types = rep('character',11),
                       widths = c(2,7,2,6,1,26,5,4,3,16,8))
  names(TI) = c("Record Identity","TIPLOC code","Capitals","NALCO","NLC Check Character","TPS Description",
                "STANOX","PO MCP Code","CRS Code","Description","Spare")
  TI$Spare = NULL
  TI$`Record Identity` = NULL
  TI = strip_whitespace(TI)

  # Add the rowid
  TI$rowID = seq(from = 1, to = length(types))[types == "TI"]

  #TIPLOC Amend
  if(!silent){message(paste0(Sys.time()," importing TIPLOC Amend"))}
  TA = raw[types == "TA"]
  TA = iotools::dstrfw(x = TA,
                       col_types = rep('character',12),
                       widths = c(2,7,2,6,1,26,5,4,3,16,7,1))
  names(TA) = c("Record Identity","TIPLOC code","Capitals","NALCO","NLC Check Character","TPS Description",
                "STANOX","PO MCP Code","CRS Code","Description","New TIPLOC","Spare")
  TA$Spare = NULL
  TA$`Record Identity` = NULL
  TA = strip_whitespace(TA)

  # Add the rowid
  TA$rowID = seq(from = 1, to = length(types))[types == "TA"]

  #TIPLOC Delete
  if(!silent){message(paste0(Sys.time()," importing TIPLOC Delete"))}
  TD = raw[types == "TA"]
  TD = iotools::dstrfw(x = TD,
                       col_types = rep('character',3),
                       widths = c(2,7,71))
  names(TD) = c("Record Identity","TIPLOC code","Spare")
  TD$Spare = NULL
  TD$`Record Identity` = NULL
  TD = strip_whitespace(TD)

  # Add the rowid
  TD$rowID = seq(from = 1, to = length(types))[types == "TD"]

  #Association
  if(!silent){message(paste0(Sys.time()," importing Association"))}
  AA = raw[types == "AA"]
  AA = iotools::dstrfw(x = AA,
                       col_types = rep('character',16),
                       widths = c(2,1,6,6,6,6,7,2,1,7,1,1,1,1,31,1))
  names(AA) = c("Record Identity","Transaction Type","Base UID","Assoc UID","Assoc Start date","Assoc End date",
                "Assoc Days","Assoc Cat","Assoc Date Ind","Assoc Location","Base Location Suffix","Assoc Location Suffix",
                "Diagram Type","Association Type","Filler","STP indicator")

  AA$Filler = NULL
  AA$`Record Identity` = NULL
  AA = strip_whitespace(AA)

  AA$`Assoc Start date` = as.Date(AA$`Assoc Start date`, format = "%d%m%y")
  AA$`Assoc End date` = as.Date(AA$`Assoc End date`, format = "%d%m%y")


  AA$`Transaction Type` = as.factor(AA$`Transaction Type`)
  AA$`Assoc Date Ind` = as.factor(AA$`Assoc Date Ind`)
  AA$`Diagram Type` = as.factor(AA$`Diagram Type`)
  AA$`Association Type` = as.factor(AA$`Association Type`)
  AA$`STP indicator` = as.factor(AA$`STP indicator`)

  AA$`Assoc Location Suffix` = as.integer(AA$`Assoc Location Suffix`)

  # Add the rowid
  AA$rowID = seq(from = 1, to = length(types))[types == "AA"]

  # Trailer Record
  # Trailer Record
  if(!silent){message(paste0(Sys.time()," importing Trailer Record"))}
  ZZ = raw[types == "ZZ"]
  ZZ = iotools::dstrfw(x = ZZ,
                       col_types = rep('character',2),
                       widths = c(2,78))
  names(ZZ) = c("Record Identity","Spare")
  ZZ$Spare = NULL
  ZZ = strip_whitespace(ZZ)

  # Add the rowid
  ZZ$rowID = seq(from = 1, to = length(types))[types == "ZZ"]

  # Prep the main files
  if(!silent){message(paste0(Sys.time()," Preparing Imported Data"))}
  stop_times = dplyr::bind_rows(list(LO,LI,LT))
  stop_times = stop_times[order(stop_times$rowID),]
  stop_times$schedule = as.integer(as.character(cut(stop_times$rowID,c(BS$rowID,ZZ$rowID[1]), labels = BS$rowID)))
  stop_times$stop_sequence = sequence(rle(stop_times$schedule)$lengths)


  BX$rowIDm1 = BX$rowID -1
  BX$rowID = NULL
  schedule = dplyr::left_join(BS,BX,by = c("rowID" = "rowIDm1"))

  results = list(stop_times,schedule,TI,TA,TD,AA,CR)
  names(results) = c("stop_times","schedule","TI","TA","TD","AA","CR")
  return(results)
}
