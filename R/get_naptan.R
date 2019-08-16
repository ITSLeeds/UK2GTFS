#' get naptan
#'
#' download the naptan stop locations
#' @param url url to naptan in csv format
#' @export
get_naptan <- function(url = "http://naptan.app.dft.gov.uk/DataRequest/Naptan.ashx?format=csv"){

  utils::download.file(url = url, destfile = "naptan.zip", mode = "wb")
  dir.create("temp")
  utils::unzip("naptan.zip", exdir = "temp")
  naptan <- read.csv("temp/stops.csv", stringsAsFactors = F)
  unlink("temp", recursive = T)
  file.remove("naptan.zip")

  #clean file
  naptan <- naptan[,c("ATCOCode","NaptanCode","CommonName","Longitude","Latitude")]
  names(naptan) <- c("stop_id","stop_code","stop_name","stop_lon","stop_lat")

  naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
  naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)

  # Append alterative tags
  extra <- naptan[naptan$stop_id %in% c("9400ZZSDBCK","9400ZZSDSTV","9400ZZSDTOT"),]
  extra$stop_id <- paste0(extra$stop_id,"0")
  naptan <- rbind(naptan, extra)

  return(naptan)
}
