#' Convert ATOC Files to GTFS
#'
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param path_in Path to ATOC File
#' @param path_out Path to where GTFS files should be saved
#' @inheritParams
#' @seealso
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' r2 = journey(from, to, apitype = "car")
#' }

atoc2gtfs <- function(path_in,path_out){

  # Is input a zip or a folder
  if(grepl(".zip",path_in)){
    # Unzip
    files <- unzip(path_in, exdir = "tmp")
    cleanup <- TRUE
  }else{
    # folder
    cleanup <- FALSE
    files <- list.files(path_in, full.names = T)
  }

  # Are all the files we would expect there?
  files.ext = substr(files, nchar(files) - 3, nchar(files))
  files.ext.need = c(".alf",".dat",".flf",".mca",".msn",".set",".tsi",".ztr")
  if(!all(files.ext.need %in% files.ext)){
    # Missing Some files
    files.ext.missing = files.ext.need[!files.ext.need %in% files.ext]
    warning(paste0("Missing files with the extension(s) ", paste(files.ext.missing, collapse = " ")))
    stop()
  }

  # Read In each File
  alf = importALF(files[grepl(".alf",files)])
  flf = importFLF(files[grepl(".flf",files)])
  file = files[grepl(".mca",files)]

}

#' Import the .alf file
#'
#' @details
#' Imports the .alf file and returns data.frame
#'
#' @param file Path to .alf file
#'
importALF <- function(file){
  nc<-max(count.fields(file, sep=","))
  table <- read.table(file = file,
                      header = FALSE,
                      sep = ",",
                      fill = TRUE,
                      col.names = c("M","O","D","T","S","E","P","F","U","R"),
                      stringsAsFactors = FALSE)

  # Now Fix Misaigned Values
  # Check each column for misalignments
  checkCol = function(x,val){
    checkCol.inner = function(x,val){substr(x,1,2) == paste0(val,"=")}
    res = sapply(x,checkCol.inner, val = val)
    return(res)
  }

  for(i in seq(1:ncol(table)) ){
    colcheck = checkCol(x = table[[i]], val = names(table)[i])
    if(all(colcheck)){
      # Simple case remove the "A="
      table[[i]] <- substr(table[[i]], 3, nchar(table[[i]]))
    }else{
      moves = ifelse(colcheck,NA,table[[i]])
      keeps = ifelse(colcheck,substr(table[[i]], 3, nchar(table[[i]])),NA)
      attributes(moves) = NULL
      attributes(keeps) = NULL

      table[[i]] = keeps

      #now loop over the moves and put in their place
      for(j in seq(1:length(moves))){
        if(!is.na(moves[j]) & !moves[j] == ""){
          # Find where it should go
          coltogo = substr(moves[j],1,1)
          if(table[j,coltogo] == ""){
            table[j,coltogo] = moves[j]
          }else{
            message(paste0("Oh now!",i," ",j))
            stop()
          }
          rm(coltogo)
        }
      }
      rm(moves,keeps)
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
#'
importFLF <- function(file){
  table = read.table(file = file,
                   sep = "",
                   stringsAsFactors = FALSE,
                   header = FALSE,
                   fill = TRUE)

  #remove end line
  if(table[nrow(table),1] == "END"){
    table = table[seq(1:(nrow(table)-1)),]
  }

  # Bind dother a few columns
  table[[1]] = paste0(table[[1]]," ",table[[2]])
  names(table) = c("Type", "del1", "Mode", "del2", "from", "del3", "to", "del4",  "time", "units")
  table = table[,c("Type", "Mode", "from", "to","time", "units")]
  return(table)
}

#' Import the .mca file
#'
#' @details
#' Imports the .mca file and returns data.frame
#'
#' @param file Path to .mca file
#'
importMCA <- function(file){
  # see https://wiki.openraildata.com/index.php/CIF_File_Format
  raw = readLines(con = file,
             n = -1)
  types = substr(raw, 1, 2)

  #break out each part of the file
  HD

  #TIPLOC Insert
  TI = raw[types == "TI"]
  TI = iotools::dstrfw(x = TI,
                        col_types = rep('character',11),
                        widths = c(2,7,2,6,1,26,5,4,3,16,8))
  names(TI) = c("Record Identity","TIPLOC code","Capitals","NALCO","NLC Check Character","TPS Description",
                "STANOX","PO MCP Code","CRS Code","Description","Spare")
  TI$Spare = NULL

  #Association
  AA = raw[types == "AA"]
  AA = iotools::dstrfw(x = AA,
                       col_types = rep('character',16),
                       widths = c(2,1,6,6,6,6,7,2,1,7,1,1,1,1,31,1))
  names(AA) = c("Record Identity","Transaction Type","Base UID","Assoc UID","Assoc Start date","Assoc End date",
                "Assoc Days","Assoc Cat","Assoc Date Ind","Assoc Location","Base Location Suffix","Assoc Location Suffix",
                "Diagram Type","Association Type","Filler","STP indicator")



  BS

  BX

  LO

  LI

  LT

  CR

  ZZ





  table = read.fwf(file = file,
                   widths = c(3,10,18,45,53),
                   skip = 1,
                   stringsAsFactors = FALSE,
                   header = FALSE
                  )


  return(table)
}
