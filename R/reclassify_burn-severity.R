# ===============================================================================
#
# Burn severity data preparation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 30 Aug 2020
# Last commit: 30 Aug 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Takes raw burn severity raster data from landsat data and reclassifies burn 
# severity into 4 groups from Parks et al. (2014). Clips reclassified data by zone
# polygon boundary. Also, reprojects raster if given.
#
# ===============================================================================
# 
# User inputs:
#
# raw_burn_file = Raster file of raw burn severity data
# prj = Target output projection
# site_boundary_file = polygon .shp file of full site boundary
# zone = vector of zone numbers
# zone_boundary_file = polygon .shp file of zone boundaries
# rcl_burn_output = Raster file name for output from rbr reclassification
# rcl_burn_zone_output = Raster file name (containing {z} glue zone placeholder) for
#   outputs from zone clipping operation
# 
# ===============================================================================
#
# RBR classification scheme:
#
# 1 = Unburned
# 2 = Low burn severity
# 3 = Medium burn severity
# 4 = High burn severity
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

raw_burn_file <- 'data/site_data/tubbs17_rbr/full_site/ppwd_L8_oct-nov2017_RBR.tif'

site_boundary_file <- 'data/site_data/ppwd_boundary/PWD_BASE_Boundary_PepperwoodPreserve_UTM_NAD83.shp'
zone <- 2:13
zone_boundary_file <- 'data/site_data/zone_shp/ppwd_zones.shp'

prj <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

rcl_burn_output <- 'data/site_data/tubbs17_rbr/full_site/ppwd_tubbs17_rbr.tif'
rcl_burn_zone_output <- 'data/site_data/tubbs17_rbr/zone/ppwd_tubbs17_rbr_z{z}.tif'

# ================ Reproject and reclassify burn severity raster ================ 

raw_burn <- raster(raw_burn_file)

ppwd_boundary <- readOGR(site_boundary_file) %>%
  spTransform(projection(raw_burn))

raw_burn <- raw_burn %>%
  crop(ppwd_boundary) %>%
  projectRaster(
    crs = prj,
    method = 'ngb'
  )

rcl_burn <- reclassify(
  x = raw_burn, 
  rcl = matrix(c(
    cellStats(raw_burn, min) - 100, 35, 1,
    35, 130, 2,
    130, 298, 3,
    298, cellStats(raw_burn, max) + 100, 4), 
    byrow = TRUE, ncol = 3
  )
)

writeRaster(
  x = rcl_burn,
  filename = rcl_burn_output,
  format = 'GTiff'
)

# ==================== Clip burn raster to zone boundaries ====================== 

zone_boundaries <- readOGR(zone_boundary_file) %>%
  spTransform(prj)

for (z in zone) {
  
  z_boundary <- subset(zone_boundaries, Zone == z)
  
  z_rcl_burn <- crop(rcl_burn, z_boundary)
  
  writeRaster(
    z_rcl_burn,
    filename = glue(rcl_burn_zone_output),
    format = 'GTiff'
  )
  
}

# ===============================================================================
