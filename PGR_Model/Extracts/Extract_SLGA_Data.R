library(slga)
library(raster)
library(rgdal)
#require(slga)

urlRoot <- "http://www.asris.csiro.au/arcgis/services"
ausData <- "ACLEP_AU_NAT_C"

productInfo <- "CLY"
## Depth to Rock DER - Metres
## Rooting Depth DPE - Metres
## Organic Carbon SOC - %
## pH Soil Water PHW
## pH Soil CaCl2 PHC
## Clay CLY - %
## Silt SLT - %
## Sand SND - %
## Available Water Capactiy AWC - %
## Total P PTO - %
## Total N NTO - %

depthInfo <- 1
## 1: 0-5cm
## 2: 5-15cm
## 3: 15-30cm
## 4: 30-60cm
## 5: 60-100cm
## 6: 100-200cm

aoi <- c(152.95, -27.55, 153.07, -27.45)
soil_url <- slga:::make_soils_url(product = 'NAT', attribute = 'CLY', component = 'VAL', depth = 1, aoi = aoi)
surface_clay <- slga:::get_slga_data(url = soil_url, out_temp = "C://temp//a.tif")

clay_content <- raster("C://temp//a.tif")

df_clay_content <- as(clay_content, "SpatialGridDataFrame")
df_clay_content@data[df_clay_content@data < 0] <- 0
plot(df_clay_content)

new_df <- cbind(coordinates(df_clay_content), df_clay_content@data)
names(new_df)[1] <- "longitude"
names(new_df)[2] <- "latitude"
names(new_df)[3] <- "clay_content"
new_df


