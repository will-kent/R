library(bomrang)
library(ggplot2)
library(sf)

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get LGA shape file
lga <- st_read('./Data Files/LGA/LGA_2019_AUST.shp')

# Plot LGA's
ggplot() +
  geom_sf(data = lga) +
  coord_sf()

# Determine centroid of LGA - note WARNING due to centroid being calculated on a 2D space.
lga <- lga %>% 
  mutate(lga_centroid = st_centroid(geometry)) %>% 
  select(-LGA_CODE19, -STE_CODE16, -AREASQKM19)

