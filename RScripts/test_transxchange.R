dir = "C:/Users/Malcolm/Downloads/Traveline/EA/"
files = list.files(dir, full.names = T)
run_debug = F

foo = pbapply::pblapply(files[1:50], transxchange_import, run_debug = run_debug)

names(foo)
bar = transxchange_import(files[4], run_debug = T)
file = files[4]


system.time(xml_data1 <- XML::xmlToList(file))
system.time(xml_data2 <- xml2::as_list(xml2::read_xml(file)))
xml_data2 = xml_data2[[1]]

StopPoints1 = xml_data1[["StopPoints"]]
StopPoints2 = xml_data2[["StopPoints"]]
identical(StopPoints1,StopPoints2)
