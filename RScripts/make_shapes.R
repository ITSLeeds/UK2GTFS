# downlaod rail map
library(sf)
library(stplanr)
library(devtools)
library(tmap)
library(RANN)
library(dodgr)
tmap_mode("view")
#devtools::install_github("ropensci/stplanr")

# sln = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways.geojson")
# sln = st_transform(sln, 27700)
# sln = SpatialLinesNetwork(sln)
#
# # Get Stations and work out nearest point on rail network
# head(station)
# stationBNG = st_transform(station, 27700)
# stationBNG = st_coordinates(stationBNG)
# linepoints = st_centroid(sln@sl)
# linepoints = st_coordinates(linepoints)
#
# near = nn2(data = linepoints, query = stationBNG, k = 1)
# near.index = near[[1]]
# near.dist = near[[2]]
#
# shortpath <- sum_network_routes(sln, 1, 5, sumvars = "length")
#
# qtm(shortpath, lines.lwd = 3)

#



