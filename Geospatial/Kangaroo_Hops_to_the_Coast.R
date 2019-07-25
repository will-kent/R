library(tmaptools) # for geocode_OSM
library(rgeos) # for readWKT
#library(maptools)
#library(sf)
library(rgdal) # for CRS
library(raster)
library(ggplot2)
library(geosphere)
library(elevatr)
library(rgbif)
library(sp)

# Define parameters
# 1. Location of interest, 2. Number of points to sample
location <- "Uluru"
pts_2_sample <- 10000

# Set up projections - IMPORTANT - confirm the regions for projections are correct
# We'll stick with WGS84 projections for this project
wgs.84 <- get_proj4("WGS84", output = "character")

# Create file name to save location information to to reduce number of calls to the web
loc_file <- paste0(location,".Rds")

# If location information previously received use existing info else use geocode_OSM
# (Open Street Maps) to get co-oridinates of location of interest and save to file
if (file.exists(loc_file)) {
  loc_df <- readRDS(loc_file)
  } else {
  loc <- geocode_OSM(location, projection = wgs.84)
  loc_df <- as.data.frame(loc$coords)
  saveRDS(loc_df, file = loc_file)
  }

# Get latitude and longitude of place of interest and create Well Known Text
longitude <- loc_df["x",]
latitude <- loc_df["y",]
loc_txt <- paste("POINT(",longitude," ",latitude,")")
loc_wkt <- readWKT(loc_txt, p4s = CRS(wgs.84))

# Check elevation of location of interest - does it look right?
elevatr::get_elev_point(loc_wkt, src = "aws")

# If it hasn't already been downloaded get coastlines file from natural earth data
coastline_file <- "coastlines.zip"
if (!file.exists(coastline_file)) {
  download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
              destfile = coastline_file)
  }

# Unzip coastline data and load into a SpatialLinesDataFrame object
unzip(zipfile = coastline_file, 
      exdir = 'ne-coastlines-10m')
coast <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp", p4s = wgs.84)

# Turn Shape file to data.frame
coast_df <- SpatialLinesDataFrame(coast,coast@data) 

# Find the minimum distance to the coast in metres, as well as the point on the coast closest
# to the location of interest
dist_line <- dist2Line(loc_wkt, coast)
closest_longitude <- dist_line[2]
closest_latitude <- dist_line[3]

#Turn the line between Uluru and the closest point on the coast into a SpatialLines object
x <- c(longitude, closest_longitude)
y <- c(latitude, closest_latitude)
xy <- cbind(x,y)

sp_line <- SpatialLines(list(Lines(Line(xy), ID = "a")))

# Break the line into n regular intervals and create a dataframe of these, adding the location
# of the point of interest along with the point on the coast
spdf <- spsample(x = sp_line, n = pts_2_sample, type = "regular") 
points <- as.data.frame(spdf)
points <- as.data.frame(rbind(xy[1,], points, xy[2,]))

# Get the distance between sample points
dist_bw_pts <- dist_line[1] / (pts_2_sample + 1)

# Plot on map with line from location of interest to the coast
ggplot() +
  geom_path(data = coast, aes(x = long, y = lat, group = group)) +
  geom_point(data = data.frame(loc_wkt), aes(x = x, y = y), colour = "red") +
  geom_path(data = points, aes(x = x, y = y), colour = "green") +
  scale_x_continuous(limits = c(110, 155)) +
  scale_y_continuous(limits = c(-45, -5)) +
  coord_fixed()
  
# Now get elevation of points, a SpatialPointsDataFrame is returned, turn that to a dataframe
# and add distance, in kiLometres, from location of interest.
elevations <- elevatr::get_elev_point(points, prj = wgs.84 ,src = "aws")
elevations_df <- as.data.frame(elevations)
elevations_df <- cbind(elevations_df, "distance" = (as.numeric(row.names(elevations_df)) - 1) * dist_bw_pts)

# Now plot the elevation profile from location of interest to coast
ggplot(elevations_df) +
  geom_line(aes(x = (distance/1000), y = elevation)) +
  scale_x_continuous(name = "Distance (km)") +
  scale_y_continuous(name = "Elevation (m)") +
  ggtitle("Elevation Profile") +
  theme_bw()

# For each point calculate the absolute elevation distance from point a to point b
first_elevation <- elevations_df[1,1]
elevations_df <- cbind(elevations_df, "next_elevation" = c(first_elevation, head(elevations_df$elevation, -1)))
elevations_df <- cbind(elevations_df, "elevation_diff" = abs(elevations_df$elevation - elevations_df$next_elevation))

sum(sqrt(dist_bw_pts^2 + elevations_df$elevation_diff^2))
# a2 + b2 = c2
