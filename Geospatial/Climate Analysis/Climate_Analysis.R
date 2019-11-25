library(bomrang)
library(ggplot2)
library(sf)
library(tidyverse)

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Find the closest station to a given point with data from the year in question
getStation <- function(year = 1950, point){
  
  longitude <- st_coordinates(point)[,1]
  latitude <- st_coordinates(point)[,2]
  
  available_stations <- sweep_for_stations(latlon = c(latitude, longitude))
  station <- available_stations %>% 
    filter(start <= year) %>% 
    top_n(n= 1, wt = desc(distance)) %>% 
    select(site)
  
  return(station$site)

}

getHistoricalAverage <- function(station_id) {
  
  if(is.na(stationid)){
    message("No station id was made available")
    stop()
  }
  
  data <- get_historical(stationid = station_id, type = c("min", "max"))
  return(data)
}

# Get LGA shape file
lga <- st_read('./Data Files/LGA/LGA_2019_AUST.shp')

# Plot LGA's
ggplot() +
  geom_sf(data = lga) +
  coord_sf()

# Determine centroid of LGA - note WARNING due to centroid being calculated on a 2D space.
lga <- lga %>% 
  filter(AREASQKM19 != 0,
         !(is.na(geometry))) %>% 
  mutate(lga_centroid = st_centroid(geometry)) %>% 
  select(-STE_CODE16, -AREASQKM19)

# Create data frame to hold station id values for each lga
station_lga <- data.frame(station_id = character(),
                      lga_code = character())

# Loop through shape file to get closest weather station to the centroid with at least x years data
for(i in 1:nrow(lga)){
  station_id <- getStation(1950, lga[i,]$lga_centroid)
  lga_code <- as.character(lga[i,]$LGA_CODE19)
  df_row <- data.frame(cbind(station_id, lga_code))
  station_lga <- rbind(station_lga, df_row)
}

get_historical(stationid = 072023, type = c("min"))
sweep_for_stations(latlon = c(-36.0266, 146.9704))