# clean naptan for gtfs
library(sf)
library(dplyr)

naptan = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/Stops.csv")
naptan = naptan[,c("ATCOCode","NaptanCode","CommonName","Latitude","Longitude","StopType")]

corpus <- jsonlite::fromJSON("D:/Users/earmmor/OneDrive - University of Leeds/NetworkRail/Reference Data/CORPUSExtract.json/corpus-out.json")
corpus <- corpus[[1]]
corpus$TIPLOC[corpus$TIPLOC == ""] <- NA
corpus$TIPLOC[corpus$TIPLOC == " "] <- NA

foo <- dplyr::left_join(naptan, corpus, by = c("ATCOCode" = "NLC"))

library(osmdata)
bb = getbb("Great Britain")
#q <- opq(bbox = c(50, -11, 61, 2)) %>%
q <- opq(bbox = c(50, -1, 52, 2)) %>%
  add_osm_feature(key = 'ref:tiploc')
res = osmdata_sf(q = q)

railrefs = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/RailReferences.csv", stringsAsFactors = F)
railrefs = railrefs[,c("AtcoCode","TiplocCode","CrsCode","StationName","Easting","Northing")]
railrefs = st_as_sf(railrefs, coords = c("Easting","Northing"), crs = 27700)
railrefs = st_transform(railrefs, 4326)
coords = as.data.frame(st_coordinates(railrefs))
names(coords) = c("stop_lat","stop_lon")
railrefs = as.data.frame(railrefs)
railrefs$geometry = NULL
railrefs = cbind(railrefs,coords)
