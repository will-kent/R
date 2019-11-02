library(slga)
library(raster)
library(rgdal)
library(readr)


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

# Products
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

# Depth Info
## 1: 0-5cm
## 2: 5-15cm
## 3: 15-30cm
## 4: 30-60cm
## 5: 60-100cm
## 6: 100-200cm

getBboxMedianValues <- function(min_long, min_lat, max_long, max_lat, output_path, file_prefix,
                                products = list("CLY","SLT","SND","SOC","PTO","NTO","AWC"), depth = 1) {
  
  # Longitude
  if(!(min_long > -180 & min_long < 180) | !(max_long > -180 & max_long < 180)){
    message("A valid longitude was not supplied. A valid longitude should be between -180 and 180.")
    stop()
  }

  # Latitude
  if(!(min_lat > -90 & min_lat < 90) | !(max_lat > -90 & max_lat < 90)){
    message("A valid latitude was not supplied. A valid latitude should be between -90 and 90.")
    stop()
  }
  
  # Output path is valid
  if(!dir.exists(output_path)){
    message("Output path not found")
    stop()
  }
  
  # Output prefix has been supplied
  if(is.na(file_prefix)){
    message("Output prefix for file not supplied")
    stop()
  }
  
  # handle products that aren't lists
  if(class(products) != "list"){
    products <- list(products)
  }
  
  # Check depth is valid, if not stop processing
  if(!(depth %in% c(1,2,3,4,5,6))){
    message("A valid depth value was not provided. A valid value is between 1 and 6, inclusive")
    stop()
  }
  
  df_data <- data.frame(prod = character(),
                        val = double())
  output_csv <- paste0(output_path,"/",file_prefix,".csv")

  aoi <- c(min_long, min_lat, max_long, max_lat)
  
  for (product in products) {
    output_tif <- paste0(output_path,"/Soil.tif") 
    soil_url <- slga:::make_soils_url(product = 'NAT', attribute = product, component = 'VAL', depth = depth, aoi = aoi)
    
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


getPolygonMedianValues <- function(polygon, output_path, output_prefix, products = list("CLY","SLT","SND","SOC","PTO","NTO","AWC"), depth = 1){
  
  # Check parameters
  # Polygon
  if(is.na(polygon)){
    stop("Valid polygon not supplied")
  }
  
  # Output path is valid
  if(!dir.exists(output_path)){
    stop("Output path not found")
  }
  
  # Output prefix has been supplied
  if(is.na(output_prefix)){
    stop("Output prefix for file not supplied")
  }
  
  # handle products that aren't lists
  if(class(products) != "list"){
    products <- list(products)
  }
  
  # Check depth is valid, if not stop processing
  if(!(depth %in% c(1,2,3,4,5,6))){
    stop("Not a valid value provided for depth")
  }
  
  # Check if output file exists
  output_csv <- paste0(output_path,"/",output_prefix,".csv")
  
  if(file.exists(output_csv)){
    print("File already exists!")
  }else{
    # Loop through products and download tif
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
      df_polygon_data <- rbind(df_polygon_data, df)
      }
    write_csv(df_polygon_data, output_csv)
  }
}