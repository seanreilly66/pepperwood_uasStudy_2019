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
# polygon boundary
#
# ===============================================================================
# 
# User inputs:
#
# raw_veg_file = Raster file of vegetation species distribution data
# zone = vector of zone numbers
# zone_boundary_file = polygon .shp file of zone boundaries
# reclassified_veg_output = Raster file name for output from reclassification
# veg_zone_output = Raster file name (containing {z} glue zone placeholder) for
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
# raster, tidyverse, glue
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
library(tidyverse)
library(glue)

# ================================= User inputs =================================

grid_metrics_file <- 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_compiled-data.csv'

# ======================== Regression modelling function ======================== 