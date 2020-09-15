# ===============================================================================
#
# Grid metrics data compilation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 16 Aug 2020
# Last commit: 21 Aug 2020
#
# Status: Complete
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Compiles grid metrics, vegetation, burn severity, and topography data into one
# large data frame for subsequent analysis
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# grid_file = File location for grid metric rasters
# uas_grid_metric_file = Raster file name for computed grid metrics from UAS data
#   (containing {z} zone glue placeholder) without individual metric name extension
# als_grid_metric_file = Raster file name for computed grid metrics from ALS data
#   (containing {z} zone glue placeholder) without individual metric name extension
# veg_file <- Vegetation class file name (containing {z} zone glue placeholder)
# rbr_file <- Tubbs fire RBR file name (containing {z} zone glue placeholder)
# topo_file <- Topography file name (containing {z} zone glue placeholder)
# out_file <- output file name (.csv)
# 
# ===============================================================================
#
# Vegetation classification:
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
# Topography classification raster:
#   1 = Valley
#   2 = Slope
#   3 = Flat
#   4 = Slope
#   5 = Slope
#   6 = Ridge
# This classification scheme combines all slope types into one group
#
# ===============================================================================
#
# Relativized burn ratio classification raster:
#   1 = Unchanged
#   2 = Low
#   3 = Moderate
#   4 = Severe
#
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, tidyverse, glue
# 
# ===============================================================================
#
# Known problems:
#
# ==============================================================================

library(sp)
library(raster)
library(tidyverse)
library(glue)

# ================================= User inputs =================================

zone <- c(2:4, 6:13) 

grid_file <- 'data/grid_metrics/rasters/'

uas_grid_metric_file <- 'ppwd_uas_z{z}_f2_hnorm-als_grid-metrics_20m-grid'
als_grid_metric_file <- 'ppwd_als_z{z}_hnorm-als_grid-metrics_20m-grid'

veg_file <- 'data/site_data/veg_class/zone/ppwd_veg_z{z}.tif'
rbr_file <- 'data/site_data/tubbs17_rbr/zone/ppwd_tubbs17_rbr_z{z}.tif'
topo_file <- 'data/site_data/topography/zone/ppwd_topo_z{z}.tif'

out_file <- 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_compiled-data.csv'

# ======== Data compilation function ===========

data_compile <- function(grid_file, uas_file, als_file, veg_file, rbr_file, topo_file) {
  
  uas <- list.files(grid_file, pattern = uas_file, full.names = TRUE) %>%
    stack()
  
  names(uas) <- paste0('uas_', str_extract(names(uas), '(?<=metrics_).+'))

  als <- list.files(grid_file, pattern = als_file, full.names = TRUE) %>%
    stack()
  
  names(als) <- paste0('als_', str_extract(names(als), '(?<=metrics_).+'))
  
  veg <- raster(veg_file) %>%
    resample(uas)
  
  names(veg) <- 'veg_class'
  
  rbr <- raster(rbr_file) %>%
    resample(uas)
  
  names(rbr) <- 'rbr_class'
  
  topo <- raster(topo_file) %>%
    resample(uas)
  
  names(topo) <- 'topo_class'
  
  output_df <- stack(uas, als, veg, rbr, topo) %>%
    as.data.frame(xy = TRUE)
  
  return(output_df)
  
}

# ======= Compile dataframe ========

z = zone[1]

message('Processing zone: ', z)

compile_data = data_compile(
  grid_file = grid_file,
  uas_file = glue(uas_grid_metric_file),
  als_file = glue(als_grid_metric_file),
  veg_file = glue(veg_file),
  rbr_file = glue(rbr_file),
  topo_file = glue(topo_file)
)

for (z in zone[-1]) {
  
  message('Processing zone: ', z)
  
  zone_data = data_compile(
    grid_file = grid_file,
    uas_file = glue(uas_grid_metric_file),
    als_file = glue(als_grid_metric_file),
    veg_file = glue(veg_file),
    rbr_file = glue(rbr_file),
    topo_file = glue(topo_file)
  )
  
  compile_data = compile_data %>%
    add_row(zone_data)
  
}

write.csv(compile_data, out_file)

# ==============================================================================
