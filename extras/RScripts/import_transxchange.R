source("R/transxchange_import4.R")
dir.in =  "C:/Users/Malcolm/Downloads/Traveline"
dir.out = "C:/Users/Malcolm/Downloads/TravelineR"
files = list.files(dir.in, full.names = T, recursive = T, pattern = ".xml")
run_debug = T
res = pbapply::pblapply(files[1:20890], transxchange_import4, export = dir.out, run_debug = run_debug)
#foo = transxchange_import3(files[1])
