# ===============================================================================
#
# Isolate point clouds from the largest contiguous stand of each forest type
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 30 July 2020
# Last commit: 2 Sept 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Extracts UAS and ALS point cloud data from the largest contiguous stand for each 
# forest type. Begins by finding stand boundaries. 
# 
# Requires UAS and ALS height normalized las files (normalized to ALS DTM)
#
# ===============================================================================
# 
# User inputs:
#
# zone = Zone number from user prompt. Used in completing filenames for las_file and 
#    standard_dtm files
# uas_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from UAS data.
# als_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from ALS data
# als_dtm = .tif file name (.las faster operation) skeleton (containing {z} zone 
#   glue placeholder) for dtm raster from ALS data
# grid_ras_out = output file location for grid metric rastersrt5fd
#
# ===============================================================================
#
# Vegetation classification raster:
#
# Vegetation classification raster should contain integer values 1 through 8 
# cooresponding to the following vegetation classes:
#   1 = Human (farm, building, vineyard, etc.)
#   2 = Grassland
#   3 = Shrubland
#   4 = Water
#   5 = Wet herbaceous
#   6 = Deciduous broadleaf
#   7 = Evergreen broadleaf
#   8 = Conifer
# 
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, lidR, tidyverse, glue
# 
# ===============================================================================
#
# Known problems:
#
# Outputs hardcoded
#
# ===============================================================================

library(lidR)
library(rgdal)
library(rgeos)
library(tidyverse)
library(glue)

# ================================= User inputs =================================

veg_file <- 'data/site_data/veg_class/full_site/ppwd_veg_reclassified.tif'
rbr_file <- 'data/site_data/tubbs17_rbr/full_site/ppwd_tubbs17_rbr.tif'

zone_boundaries_file <- 'data/site_data/zone_shp/ppwd_zones_50m-buffer.shp'

uas_las_file <- 'D:/data/las/uas/ppwd_uas_z{z}_f2_hnorm-als.las'
als_las_file <- 'data/las/als/ppwd_als_z{z}_hnorm-als.las'

# ============= Identify largest contiguous unburned forest patches ============= 

veg <- raster(veg_file)

rbr <- raster(rbr_file) %>%
  resample(veg)

zones <- readOGR(zone_boundaries_file) %>%
  subset(Zone %in% c(2:4, 6:13))

veg <- veg %>%
  mask(zones) %>%
  mask(
    veg %in% 6:8,
    maskvalue = 0) %>%
  mask(
    rbr %in% 0:2,
    maskvalue = 0)

veg_poly <- veg %>%
  rasterToPolygons(na.rm = TRUE)

decid_poly <- subset(veg_poly, ppwd_veg_reclassified == 6) %>%
  buffer() %>%
  disaggregate()
evrgrn_poly <- subset(veg_poly, ppwd_veg_reclassified == 7) %>%
  buffer() %>%
  disaggregate()
conifer_poly <- subset(veg_poly, ppwd_veg_reclassified == 8) %>%
  buffer() %>%
  disaggregate()

decid_poly$area <- area(decid_poly)
evrgrn_poly$area <- area(evrgrn_poly)
conifer_poly$area <- area(conifer_poly)

decid_poly <- subset(decid_poly, area == max(area))
evrgrn_poly <- subset(evrgrn_poly, area == max(area))
conifer_poly <- subset(conifer_poly, area == max(area))

writeOGR(
  obj = decid_poly,
  dsn = 'data/site_data/veg_class/unburned_forest_stands',
  layer = 'ppwd_deciduous_unburned-forest-stand',
  driver = 'ESRI Shapefile',
  overwrite_layer = TRUE
)

writeOGR(
  obj = evrgrn_poly,
  dsn = 'data/site_data/veg_class/unburned_forest_stands',
  layer = 'ppwd_evergreen_unburned-forest-stand',
  driver = 'ESRI Shapefile',
  overwrite_layer = TRUE
)

writeOGR(
  obj = conifer_poly,
  dsn = 'data/site_data/veg_class/unburned_forest_stands',
  layer = 'ppwd_conifer_unburned-forest-stand',
  driver = 'ESRI Shapefile',
  overwrite_layer = TRUE
)

rm(rbr, veg, veg_poly, rbr_file, veg_file, zone_boundaries_file)

# ======================= Extract las data by forest type =======================

forest_clip <- function(las_file, forest_shp, zone_shp = zones) {
  
  z <- zone_shp$Zone[!is.na(over(zone_shp, forest_shp))]
  
  las <- glue(las_file) %>%
    readLAS(select = '') %>%
    clip_roi(forest_shp)
  
  return(las)
  
}

decid_uas <- forest_clip(uas_las_file, decid_poly)
writeLAS(decid_uas, 'data/las/by_veg_type/ppwd_uas_f2_deciduous_unburned-forest-stand.las')

evrgrn_uas <- forest_clip(uas_las_file, evrgrn_poly)
writeLAS(evrgrn_uas, 'data/las/by_veg_type/ppwd_uas_f2_evergreen_unburned-forest-stand.las')

conifer_uas <- forest_clip(uas_las_file, conifer_poly)
writeLAS(conifer_uas, 'data/las/by_veg_type/ppwd_uas_f2_conifer_unburned-forest-stand.las')


decid_als <- forest_clip(als_las_file, decid_poly)
writeLAS(decid_als, 'data/las/by_veg_type/ppwd_als_deciduous_unburned-forest-stand.las')

evrgrn_als <- forest_clip(als_las_file, evrgrn_poly)
writeLAS(evrgrn_als, 'data/las/by_veg_type/ppwd_als_evergreen_unburned-forest-stand.las')

conifer_als <- forest_clip(als_las_file, conifer_poly)
writeLAS(conifer_als, 'data/las/by_veg_type/ppwd_als_conifer_unburned-forest-stand.las')

# ================== Extract height data for each forest type =================== 

write.csv(decid_uas$Z, 'data/pntcld_vertical_density/ppwd_deciduous_uas.csv')
write.csv(decid_als$Z, 'data/pntcld_vertical_density/ppwd_deciduous_als.csv')

write.csv(evrgrn_uas$Z, 'data/pntcld_vertical_density/ppwd_evrgrn_uas.csv')
write.csv(evrgrn_als$Z, 'data/pntcld_vertical_density/ppwd_als_height_evergreen_unburned-forest-stand.csv')

write.csv(conifer_uas$Z, 'data/pntcld_vertical_density/ppwd_conifer_uas.csv')
write.csv(conifer_als$Z, 'data/pntcld_vertical_density/ppwd_conifer_als.csv')

# ===============================================================================
