library(tmaptools) # for geocode_OSM
library(rgeos) # for readWKT
#library(maptools)
#library(sf)
library(rgdal) # for CRS
#library(raster)
library(ggplot2)
library(geosphere)

# Set up projections - IMPORTANT - confirm the regions for projections are correct 3577 covers all of Australia
#epsg.3577 <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Use geocode_OSM (Open Street Maps) to get co-oridinates of Uluru
loc <- geocode_OSM("Uluru", projection = wgs.84)

longitude <- loc$coords["x"]
latitude <- loc$coords["y"]

loc_txt <- paste("POINT(",longitude," ",latitude,")")
loc_wkt <- readWKT(loc_txt, p4s = CRS(wgs.84))

# Download coastlines file from natural earth data and unzip
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
              destfile = 'coastlines.zip')

unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')

coast <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp", p4s = wgs.84)
coast
# Turn Shape file to data.frame
coast_df <- SpatialLinesDataFrame(coast,coast@data) 

# Plot dataframe on a map
ggplot() +
  geom_path(data = coast, aes(x = long, y = lat, group = group)) +
  coord_fixed()

# gDistance uses WGS84 projection so units are in degrees
min_degrees <- gDistance(loc_wkt,coast)
min_degrees

dist_line <- dist2Line(loc_wkt, coast)
closest_longitude <- dist_line[2]
closest_latitude <- dist_line[3]

# Transform the WGS84 longlat projections to EPSG 3577 - http://spatialreference.org/ref/epsg/3577/
#coast_3577 <- spTransform(coast,CRS(epsg.3577))
#loc_3577 <- spTransform(loc_wkt, CRS(epsg.3577))

# Find minimum distance to the coast
#dist <- gDistance(loc_3577,coast_3577)

# Stole this bit of code from - https://stackoverflow.com/questions/27384403/calculating-minimum-distance-between-a-point-and-the-coast-in-the-uk
# Creates a nice chart along with finding the closest point on the coast
th     <- seq(0,2*pi,len=1000)
circle <- cbind(1.00001*min_degrees*cos(th)+loc_wkt$x,1.00001*min_degrees*sin(th)+loc_wkt$y)
#sp_circle <- SpatialLines(list(Lines(list(Line(circle)),ID="1")),proj4string=CRS(wgs.84))

# Plot on map
#plot(coast, asp=1)
#plot(loc_wkt, add = TRUE, col = "red", pch = 20)
#plot(sp_circle, add = TRUE, col = "blue", lty = 2)

loc_df <- data.frame(loc_wkt)
data.frame(circle)
ggplot() +
  geom_path(data = coast, aes(x = long, y = lat, group = group)) +
  geom_path(data = data.frame(circle), aes(x = X1, y = X2), colour = "blue") + 
  geom_point(data = data.frame(loc_wkt), aes(x = x, y = y), colour = "red") +
  scale_x_continuous(limits = c(110, 155)) +
  scale_y_continuous(limits = c(-45, -5)) +
  coord_fixed()
