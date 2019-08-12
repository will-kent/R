library(slga)
library(raster)
library(rgdal)
library(readr)
#require(slga)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

getPaddockMedianValues <- function(min_long, min_lat, max_long, max_lat, output_path, file_prefix) {
  df_data <- data.frame(prod = character(),
                        val = double())
  output_csv <- paste0(output_path,"/",file_prefix,".csv")
  
  products <- list("CLY","SLT","SND","SOC","PTO","NTO","AWC")
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

  aoi <- c(min_long, min_lat, max_long, max_lat)
  
  for (product in products) {
    output_tif <- paste0(output_path,"/Soil.tif") 
    soil_url <- slga:::make_soils_url(product = 'NAT', attribute = product, component = 'VAL', depth = 1, aoi = aoi)
    
    if (file.exists(output_tif)){
      file.remove(output_tif)
    }
    
    download_data <- slga:::get_slga_data(url = soil_url, out_temp = output_tif)
    raster_data <- raster(output_tif)
    sgdf_data <- as(raster_data, "SpatialGridDataFrame")
    val <- median(sgdf_data@data$Soil)
    df <- data.frame(cbind(product, val))
    df_data <- rbind(df_data, df)
  }
  write_csv(df_data, output_csv)

}

getSoilDetails(152.691, -30.99, 152.693, -30.98, getwd(),"Dads-Paddock")

