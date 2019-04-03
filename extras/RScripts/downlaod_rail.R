# downlaod rail map
library(sf)
library(stplanr)
library(devtools)
library(tmap)
tmap_mode("view")
#devtools::install_github("ropensci/stplanr")

sln = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways.geojson")
sln = SpatialLinesNetwork(sln)


shortpath <- sum_network_routes(sln, 1, 5, sumvars = "length")

qtm(shortpath, lines.lwd = 3)




