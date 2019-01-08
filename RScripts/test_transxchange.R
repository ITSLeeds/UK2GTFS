source("R/transxchange_import.R")
source("R/transxchange_import2.R")
source("R/transxchange_import3.R")
dir = "C:/Users/Malcolm/Downloads/Traveline/L/"
files = list.files(dir, full.names = T, recursive = T, pattern = ".xml")
run_debug = T
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
