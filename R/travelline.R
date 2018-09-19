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
Operators = xml_data["Operators"]
Operators = Operators[[1]]
Operators <- data.frame(matrix(unlist(Operators), nrow=length(Operators), byrow=T),stringsAsFactors=FALSE)


Services = xml_data["Services"]
VehicleJourneys = xml_data["VehicleJourneys"]
#attrs = xml_data[".attrs"]
