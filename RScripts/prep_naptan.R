# clean naptan for gtfs
library(sf)

naptan = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/Stops.csv")
naptan = naptan[,c("ATCOCode","NaptanCode","CommonName","Latitude","Longitude","StopType")]

railrefs = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/RailReferences.csv", stringsAsFactors = F)
railrefs = railrefs[,c("AtcoCode","TiplocCode","CrsCode","StationName","Easting","Northing")]
railrefs_coords = st_as_sf(railrefs, coords = c("Easting","Northing"), crs = 27700)
