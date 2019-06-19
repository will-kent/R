library(tmaptools) # for geocode_OSM
library(rgeos) # for readWKT
#library(maptools)
#library(sf)
library(rgdal) # for CRS
library(raster)
library(ggplot2)
library(geosphere)

# Set up projections - IMPORTANT - confirm the regions for projections are correct 3577 covers all of Australia
#epsg.3577 <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
wgs.84 <- get_proj4("WGS84", output = "character")

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
summary(coast)
# Turn Shape file to data.frame
coast_df <- SpatialLinesDataFrame(coast,coast@data) 

# gDistance uses WGS84 projection so units are in degrees
min_degrees <- gDistance(loc_wkt,coast)
min_degrees

dist_line <- dist2Line(loc_wkt, coast)
closest_longitude <- dist_line[2]
closest_latitude <- dist_line[3]

#Turn the line between Uluru and the closest point on the coast into a SpatialLines object
x <- c(longitude, closest_longitude)
y <- c(latitude, closest_latitude)
xy <- cbind(x,y)

sp_line <- SpatialLines(list(Lines(Line(xy), ID = "a")))

#Download Digital Elevation Model (DEM) from Geoscience Australia - this is 25 metre grid
download.file("http://pid.geoscience.gov.au/dataset/ga/89676/export-full-1560937645581.zip",
              destfile = "Australia_25_")
?extract
extract(sp_line)
