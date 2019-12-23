library(bomrang)
library(ggplot2)
library(sf)
library(tidyverse)

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
firstYear = 1950

getYears <- function(station_id, type){
  
  minYear = data.frame(min_year = 2000)
  maxYear = data.frame(max_year = 2000)
  
  tryCatch({gh <- get_historical(stationid = station_id, type = type)
  
  minYear <- gh %>% 
    filter(min_temperature != "NA") %>% 
    group_by(station_number) %>% 
    summarise(min_year = min(year))
  
  maxYear <- gh %>% 
    filter(min_temperature != "NA") %>% 
    group_by(station_number) %>% 
    summarise(max_year = max(year))},
  error = function(e){})

  return(c(minYear, maxYear))
}

# Find the closest station to a given point with data from the year in question
getStation <- function(year = 1950, point){
  
  longitude <- st_coordinates(point)[,1]
  latitude <- st_coordinates(point)[,2]
  
  available_stations <- sweep_for_stations(latlon = c(latitude, longitude))
  station <- available_stations %>% 
    filter(start <= year) %>% 
    select(site)
    
  return(station$site)

}

getHistoricalTemps <- function(station_id, start_year = 1950) {
  
  if(is.na(station_id)){
    message("No station id was made available")
    stop()
  }
  
  min_temps <- get_historical(stationid = station_id, type = "min")
  max_temps <- get_historical(stationid = station_id, type = "max")
  
  avg_temp <- min_temps %>% 
    inner_join(max_temps, by = c("station_number", "year", "month", "day")) %>% 
    select(station_number, year, month, day, min_temperature, max_temperature) %>% 
    filter(min_temperature != "NA",
           max_temperature != "NA",
           year >= firstYear) %>% 
    mutate(mean_daily = (min_temperature + max_temperature) / 2)
  
  return(avg_temp)
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
  stations <- getStation(firstYear, lga[i,]$lga_centroid)
  for(j in stations){
    years <- getYears(station_id = j, type = "min")
    if((years$min_year <= firstYear) & (years$max_year >= as.integer(format(Sys.Date(), "%Y")))){
      station_id <- j
      message("Using station ", as.character(station_id), " for analysis")
      break
    }
  }
  lga_code <- as.character(lga[i,]$LGA_CODE19)
  df_row <- data.frame(cbind(station_id, lga_code))
  station_lga <- rbind(station_lga, df_row)
}

# write file to disk so we don't need to do it again.
write_csv(station_lga, "./Data Files/LGA_to_station.csv")

# Create data frame to hold station id values for each lga
station_means <- data.frame(station_id = character(),
                          year = integer(),
                          period_mean = numeric(),
                          annual_mean = numeric())

# Get the historical average for the weather station
for(k in 1:nrow(station_lga)){
  
  station <- station_lga[k,]$station_id
  if(!(station %in% station_means$station_id)){
    temps <- getHistoricalTemps(station_id = station, firstYear)
    
    temp_baseline <- avg_temp %>% 
      filter(between(year, firstYear, firstYear+50)) %>% 
      summarise(period_mean = mean(mean_daily))
    
    mean_annual <- avg_temp %>% 
      group_by(station_number, year) %>%
      summarise(annual_mean = mean(mean_daily))
    
    df_means <- temp_baseline %>%
      merge(mean_annual, all = TRUE) %>% 
      select(station_id = station_number,
             year,
             period_mean,
             annual_mean)
    
    station_means <- rbind(station_means, df_means)
    
  }
  
}

b <- getYears(station_id = 066051, type = "min")
a <- getYears(station_id = 069049, type = "min")

gh <- get_historical(stationid = 069049, type = "min")

