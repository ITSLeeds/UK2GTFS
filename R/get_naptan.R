#' get naptan
#'
#' download the naptan stop locations
#' @param url url to naptan in csv format
#' @export
get_naptan <- function(url = "http://naptan.app.dft.gov.uk/DataRequest/Naptan.ashx?format=csv") {
  utils::download.file(url = url, destfile = "naptan.zip", mode = "wb")
  dir.create("temp")
  utils::unzip("naptan.zip", exdir = "temp")
  naptan <- utils::read.csv("temp/stops.csv", stringsAsFactors = F)
  unlink("temp", recursive = T)
  file.remove("naptan.zip")

  # clean file
  naptan <- naptan[, c("ATCOCode", "NaptanCode", "CommonName", "Longitude", "Latitude")]
  names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lon", "stop_lat")

  naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
  naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)

  # Append alterative tags
  extra <- naptan[naptan$stop_id %in% c("9400ZZSDBCK", "9400ZZSDSTV", "9400ZZSDTOT"), ]
  extra$stop_id <- paste0(extra$stop_id, "0")
  naptan <- rbind(naptan, extra)

  # Append Missing Stops
  stop_missing1 <- c("9400ZZLUHPC2", "9400ZZLUBNK8","9400ZZRLWLN1", "9400ZZRLWBY1", "9400ZZRLHHL1")
  alt_ids       <- c("9400ZZLUHPC", "9400ZZLUBNK","9400ZZRLWLN", "9400ZZRLWBY", "9400ZZRLHHL")
  extra <- naptan[naptan$stop_id %in% alt_ids, ]
  extra$stop_id <- stop_missing1
  naptan <- rbind(naptan, extra)

  stop_missing2 <- c("490000345Z","910GWATRLMN", "490005204Z","490G000117")
  extra <- naptan[1:4, ]
  extra$stop_id <- stop_missing2
  extra$stop_code <- ""
  extra$stop_name <- c("Hallsville Road","Waterloo Mainline", "Chislehurst Sainsbury's","Colonnades Leisure Park")
  extra$stop_lon <- c("0.011065","-0.112220", "0.066852","-0.116035")
  extra$stop_lat <- c("51.513027","51.503703", "51.418832","51.356938")
  naptan <- rbind(naptan, extra)

  return(naptan)
}
