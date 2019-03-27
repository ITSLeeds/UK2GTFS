# get naptan
get_naptan <- function(url = "http://naptan.app.dft.gov.uk/DataRequest/Naptan.ashx?format=csv"){

  download.file(url = url, destfile = "naptan.zip", mode = "wb")
  dir.create("temp")
  unzip("naptan.zip", exdir = "temp")
  naptan <- read.csv("temp/stops.csv", stringsAsFactors = F)
  unlink("temp", recursive = T)
  file.remove("naptan.zip")

  #clean file
  naptan <- naptan[,c("ATCOCode","NaptanCode","CommonName","Longitude","Latitude")]
  names(naptan) <- c("stop_id","stop_code","stop_name","stop_lon","stop_lat")

  naptan$stop_lon <- round(naptan$stop_lon, 6)
  naptan$stop_lat <- round(naptan$stop_lat, 6)
  return(naptan)
}
