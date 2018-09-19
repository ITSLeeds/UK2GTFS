# travelline 
data = XML::xmlParse("xmlToList")
xml_data = XML::xmlToList(data)

StopPoints = xml_data["StopPoints"]
StopPoints = StopPoints[[1]]
StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=FALSE)

RouteSections = xml_data["RouteSections"]
Routes = xml_data["Routes"]
Routes = Routes[[1]]
Routes <- data.frame(matrix(unlist(Routes), nrow=length(Routes), byrow=T),stringsAsFactors=FALSE)


JourneyPatternSections = xml_data["JourneyPatternSections"]
JourneyPatternSections = JourneyPatternSections[[1]]
foo = JourneyPatternSections[[1]]

Operators = xml_data["Operators"]
Operators = Operators[[1]]
Operators <- data.frame(matrix(unlist(Operators), nrow=length(Operators), byrow=T),stringsAsFactors=FALSE)


Services = xml_data["Services"]
Services = Services[[1]][[1]]
#Services <- data.frame(matrix(unlist(Services), nrow=length(Services), byrow=T),stringsAsFactors=FALSE)



VehicleJourneys = xml_data["VehicleJourneys"]
#attrs = xml_data[".attrs"]
