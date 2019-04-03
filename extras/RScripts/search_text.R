dir.in =  "C:/Users/Malcolm/Downloads/Traveline/EA"
files = list.files(dir.in, full.names = T, recursive = T, pattern = ".xml")
seach_text <- function(file, text){
  obj <- readLines(file, warn = F)
  fnd <- grep(text,obj)
  if(length(fnd) > 0){
    message(paste0("Text Found in ",file))
  }
}

seach_text(files[1],"RunTime")
pbapply::pblapply(files, seach_text, text = "VehicleJourneyTimingLink" )
file = "C:/Users/Malcolm/Downloads/Traveline/EA/suf_1-101-_-y08-6.xml"
