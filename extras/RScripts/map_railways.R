library(osmar)
url <- "https://download.geofabrik.de/europe/british-isles-latest.osm.pbf"

download.file(url)


src <- osmsource_file(file = "E:/OneDrive - University of Leeds/Routing/osm/british-isles-latest.osm.pbf")
map <- get_osm(complete_file(), src)









# Download the raid network

library(osmdata)
library(sf)

# Make a bounding box
box = st_sf(a = 1:2,
            geom = st_sfc(st_point(c(-6.606,49.573)),
                          st_point(c(0.425,61.157))),
            crs = 4326)

box = st_sf(a = 1:2,
            geom = st_sfc(st_point(c(-1.000,51.000)),
                          st_point(c(0.000,52.000))),
            crs = 4326)

#Download data
q1 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "rail")
q2 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "light_rail")
q3 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "funicular")
q4 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "monorail")
q5 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "subway")
q6 = opq(st_bbox(box)) %>%
  add_osm_feature(key = "railway", value = "tram")

res = c(osmdata_sf(q1),
        osmdata_sf(q2),
        osmdata_sf(q3),
        osmdata_sf(q4),
        osmdata_sf(q5),
        osmdata_sf(q6)
        )

length(res)

names(res)

osm_points = res[["osm_points"]]
osm_lines = res[["osm_lines"]]
osm_polygons = res[["osm_polygons"]]
osm_multilines = res[["osm_multilines"]]
osm_multipolygons = res[["osm_multipolygons"]]


summary(osm_lines$railway)
summary(osm_polygons$railway)
summary(osm_multipolygons$railway)

qtm(osm_polygons)
qtm(osm_multilines, lines.col = "red")
qtm(osm_lines, lines.col = "red")
