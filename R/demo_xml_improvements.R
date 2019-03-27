StopPoints_raw = xml2::xml_child(xml,"d1:StopPoints")

import_stoppoints <- function(StopPoints){
  StopPointRef       <- xml_text(xml_find_all(StopPoints, ".//d1:StopPointRef"))
  CommonName         <- xml_text(xml_find_all(StopPoints, ".//d1:CommonName"))
  Indicator          <- xml_text(xml_find_all(StopPoints, ".//d1:Indicator"))
  LocalityName       <- xml_text(xml_find_all(StopPoints, ".//d1:LocalityName"))
  LocalityQualifier  <- xml_text(xml_find_all(StopPoints, ".//d1:LocalityQualifier"))

  StopPoints <- data.frame(StopPointRef = StopPointRef,
                           CommonName = CommonName,
                           Indicator = Indicator,
                           LocalityName = LocalityName,
                           LocalityQualifier = LocalityQualifier)
  return(StopPoints)
}

t1 <- Sys.time()
StopPoints = xml2::as_list(StopPoints_raw)
# Sometimes the Indicator variaible is missing
if(!all(lengths(StopPoints) == 5)){
  sp_clean = function(sp){
    if(is.null(sp$Indicator)){
      sp$Indicator <- NA
    }
    sp <- sp[c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")]
    return(sp)
  }
  StopPoints = lapply(StopPoints,sp_clean)
}

StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=T)
names(StopPoints) <- c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")

t2 <- Sys.time()

StopPoints2 <- import_stoppoints(StopPoints_raw)

t3 <- Sys.time()

difftime(t2,t1)
difftime(t3,t2)
