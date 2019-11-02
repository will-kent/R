library(bomrang)

#getwd()
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############
# Parameters #
##############
#longitude - longitude of point in WGS84
#latitude - latitude of point in WGS84
#measurement - one of rain, min, max or solar. combining multiple measurements into one is not allowed.

#returns a bomrang.tbl, data.frame

getClimateData <- function(longitude, latitude, measurement) {
  
  if(!(longitude > -180 & longitude < 180)){
    message("A valid longitude was not supplied. Values should be between -180 and 180")
    stop()
  }
  
  if(!(latitude > -90 & latitude < 90)){
    message("A valid latitude was not supplied. Values should be between -90 and 90")
    stop()
  }
  
  #Concatenate latlong
  coord <- c(latitude, longitude)
  
  if(length(measurement) != 1){
    message("More than one measurement type supplied. Valid values are either Rain, Min, Max or Solar.")
    stop()
  }
  
  if(!(measurement %in% c("rain", "min", "max", "solar"))){
    message("A valid measurement type was not supplied")
    stop()
  }

  #Find the nearest weather station
  stations <- sweep_for_stations(latlon = coord)
  station <- stations[1,]$site
  
  if(stations[1,]$distance > 500){
    message("The closest weather station is over 500 kilometres away. Please select another location")
    stop()
    }else{
        tryCatch( { data <- get_historical(stationid = station, type = measurement) }
                  ,error = function(x){
                    msg <- paste0("Unable to process station id: ",station)
                    message(msg)
                    stop()})
    }
  
  return(data)
  
}
