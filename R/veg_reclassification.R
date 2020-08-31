# ===============================================================================
#
# Vegetation class data preparation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Aug 2020
# Last commit: 26 Aug 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Takes vegetation species distribution raster data from Ackerly et al. (2020) and
# reclassifies vegetation into eight broad groups. Clips reclassified data by zone
# polygon boundary. Also, reprojects raster if given.
#
# ===============================================================================
# 
# User inputs:
#
# raw_veg_file = Raster file of vegetation species distribution data
# prj = Target output projection
# zone = vector of zone numbers
# zone_boundary_file = polygon .shp file of zone boundaries
# rcl_veg_output = Raster file name for output from reclassification
# rcl_veg_zone_output = Raster file name (containing {z} glue zone placeholder) for
#   outputs from zone clipping operation
# 
# ===============================================================================
#
# Input species classification scheme:
#
# 1 = builtup
# 3 = grassland
# 4 = orchard
# 5 = shrubland
# 6 = vineyard
# 7 = water
# 8 = wet herbaceous
# 11 = big-leaf maple
# 12 = black oak
# 13 = blue oak
# 14 = bay laurel
# 15 = buckeye
# 16 = coast live oak
# 17 = redwood
# 18 = douglas fir
# 19 = oregon white oak
# 20 = madrone
# 21 = tanoak
# 22 = valley oak
#
# ===============================================================================
#
# New group classification scheme:
#
# 1 = Human (builtup, vineyard)
# 2 = Grassland
# 3 = Shrubland
# 4 = Water
# 5 = Wet herbaceous
# 6 = Deciduous broadleaf (big-leaf maple, black oak, blue oak, buckeye, oregon white oak, valley oak)
# 7 = Evergreen broadleaf (bay laurel, coast live oak, madrone, tanoak)
# 8 = Conifer (redwood, douglas fir)
# 
# ===============================================================================
# 
# Package dependences: 
#
# raster, tidyverse, glue, rgdal
# 
# ===============================================================================
#
# Known problems:
#
# Documentation incomplete. 
# Output location hardcoded. 
# Script incomplete.
#
# ===============================================================================

library(raster)
library(rgdal)
library(tidyverse)
library(glue)

# ================================= User inputs =================================

raw_veg_file <- 'data/site_data/veg_class/full_site/svm_avirisng_merged_spectral_metrics_class.tif'

zone <- 2:13
zone_boundary_file <- 'data/site_data/zone_shp/ppwd_zones.shp'

prj <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

rcl_veg_output <- 'data/site_data/veg_class/full_site/ppwd_veg_reclassified.tif'
rcl_veg_zone_output <- 'data/site_data/veg_class/zone/ppwd_veg_z{z}.tif'

# ================= Reproject and reclassify vegetation raster ==================

raw_veg <- raster(raw_veg_file) %>%
  projectRaster(
    crs = prj,
    method = 'ngb'
  )

rcl_veg <- reclassify(
  x = raw_veg, 
  rcl = matrix(c(
    1, 1, # builtup
    3, 2, # grassland
    4, 1, # orchard
    5, 3, # shrubland
    6, 1, # vineyard
    7, 4, # water
    8, 5, # wet herbaceous
    11, 6, # big-leaf maple
    12, 6, # black oak
    13, 6, # blue oak
    14, 7, # bay laurel
    15, 6, # buckeye
    16, 7, # coast live oak
    17, 8, # redwood
    18, 8, # douglas fir
    19, 6, # oregon white oak
    20, 7, # madrone
    21, 7, # tanoak
    22, 6), # valley oak
    byrow = TRUE, ncol = 2
  )
)

writeRaster(
  x = rcl_veg,
  filename = rcl_veg_output,
  format = 'GTiff'
)

# ===================== Clip veg raster to zone boundaries ====================== 

zone_boundary <- readOGR(zone_boundary_file) %>%
  spTransform(prj)

for (z in zone) {
  
  z_bound <- subset(zone_boundary, Zone == z)
  
  zone_veg <- crop(rcl_veg, z_bound)
  
  writeRaster(
    zone_veg,
    filename = glue(rcl_veg_zone_output),
    format = 'GTiff'
  )
  
}

# ===============================================================================
