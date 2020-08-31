# ===============================================================================
#
# Vegetation class data preparation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Aug 2020
# Last commit: 31 Aug 2020
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
# 1   Builtup
# 3   Grassland
# 4   Orchard
# 5   Shrubland
# 6   Vineyard
# 7   Water
# 8   Wet herbaceous
# 11	ACMA	Acer macrophyllum	(Big leaf maple)
# 12	AECA	Aesculus california	(Buckeye)
# 13	ARME	Arbutus menziesii	(Madrone)
# 14	NODE	Notholithocarpus densiflorus	(Tanoak)
# 15	PSME	Pseudotsuga menziesii	(Douglas fir)
# 16	QUAG	Quercus agrifolia	(Coast live oak)
# 17	QUDO	Quercus douglasii	(Blue oak)
# 18	QUGA	Quercus garryana	(Oregon oak)
# 19	QUKE	Quercus kelloggii	(Black oak)
# 20	QULO	Quercus lobata	(Valley oak)
# 21	SESE	Sequoia sempervirens	(Redwood)
# 22	UMCA	Umbellularia californica	(Bay laurel)
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
    12, 6, # buckeye
    13, 7, # madrone
    14, 7, # tanoak
    15, 8, # douglas fir
    16, 7, # coast live oak
    17, 6, # blue oak
    18, 6, # oregon oak
    19, 6, # black oak
    20, 6, # valley oak
    21, 8, # redwood
    22, 7), # bay laurel
    byrow = TRUE, ncol = 2
  )
)

writeRaster(
  x = rcl_veg,
  filename = rcl_veg_output,
  format = 'GTiff',
  overwrite = TRUE
)

# ===================== Clip veg raster to zone boundaries ====================== 

zone_boundaries <- readOGR(zone_boundary_file) %>%
  spTransform(prj)

for (z in zone) {
  
  z_boundary <- subset(zone_boundaries, Zone == z)
  
  z_rcl_veg <- crop(rcl_veg, z_boundary)
  
  writeRaster(
    z_rcl_veg,
    filename = glue(rcl_veg_zone_output),
    format = 'GTiff',
    overwrite = TRUE
  )
  
}

# ===============================================================================
