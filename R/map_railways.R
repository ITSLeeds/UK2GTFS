# Download the raid network

library(osmdata)
library(sf)

# Make a bounding box
box = st_sf(a = 1:2,
            geom = st_sfc(st_point(c(-6.606,49.573)),
                          st_point(c(0.425,61.157))),
            crs = 4326)

box = st_sf(a = 1:2,
            geom = st_sfc(st_point(c(-1.606,50.573)),
                          st_point(c(0.425,52.157))),
            crs = 4326)

#Download data
q = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway")
res = osmdata_sf(q = q)

