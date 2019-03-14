# source("R/transxchange_import.R")
# source("R/transxchange_import2.R")
# source("R/transxchange_import3.R")
source("R/transxchange_import5.R")
source("R/transxchange2gtfs.R")
source("R/get_cal.R")
source("R/write_gtfs.R")
source("R/get_naptan.R")

dir = "E:/OneDrive - University of Leeds/Routing/TransitExchangeData/data_20180515"
files = list.files(dir, full.names = T, recursive = T, pattern = ".xml")
file = files[1]
run_debug = T
naptan = get_naptan()
cal = get_bank_holidays()


x = 8
res_single = transxchange_import5(files[x], run_debug = run_debug)
gtfs_single = transxchange2gtfs(obj = res_single, run_debug = T, cal = cal, naptan = naptan)
write_gtfs(gtfs = gtfs_single, folder = "export", name = gsub(".xml","",strsplit(files[x], "/")[[1]][7]))

y = 1:10
res_batch = lapply(files[y], transxchange_import5, run_debug = run_debug)
gtfs_single = pbapply::pblapply(res_batch, transxchange2gtfs, run_debug = T, cal = cal, naptan = naptan)





saveRDS(res_single, "example_import.Rds")

Services_main <- res_single$Services_main
routes <- gtfs_single$routes

read.csv(stringsAsFactors = )

cl = parallel::makeCluster(4)
res = pbapply::pblapply(files[1:100], transxchange_import3, run_debug = run_debug, cl = cl)
parallel::stopCluster(cl)

names(res)
file = "C:/Users/Malcolm/Downloads/Traveline/L/tfl_1-CEN-_-y05-690544.xml"
system.time(res3 <- transxchange_import3(file, run_debug = T))
system.time(res1 <- transxchange_import(file,  run_debug = T))
system.time(res2 <- transxchange_import2(file, run_debug = T))

file = files[7]


system.time(xml_data1 <- XML::xmlToList(file))
system.time(xml_data2 <- xml2::as_list(xml2::read_xml(file)))
xml_data2 = xml_data2[[1]]

StopPoints1 = xml_data1[["StopPoints"]]
StopPoints2 = xml_data2[["StopPoints"]]
identical(StopPoints1,StopPoints2)
