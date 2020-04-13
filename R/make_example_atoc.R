# file = "C:/Users/malco/Downloads/ttis585/ttisf585.mca"
# raw <- readLines(con = file,  n = -1)
# types <- substr(raw, 1, 2)
#
# table(types)
# raw2 <- raw[types %in% c("TI","AA","CR")]
#
# startrows <- seq(1,length(raw))[types == "BS"]
# startrows <- startrows[65000:85000]
# #50-90 w, 50k-70 f, 70-80 f, 80-90 f, 70-90 f, 60-90 w, 65-85w
# raw3 <- raw[seq(startrows[1], startrows[length(startrows)] - 1)]
# raw3 <- c(raw[1],raw2, raw3,raw[length(raw)])
#
# table(substr(raw3, 1, 2))
# head(raw3)
# tail(raw3)
#
# write(raw3,"inst/extdata/atoc/example.mca")
#
# gtfs <- atoc2gtfs(path_in = "C:/Users/malco/Documents/GitHub/itsleeds/UK2GTFS/inst/extdata/atoc",
#                   path_out = file_path,
#                   name = "gtfs_atoc",
#                   ncores = 1,
#                   silent = FALSE)
