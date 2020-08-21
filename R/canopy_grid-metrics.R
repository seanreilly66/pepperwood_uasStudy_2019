# ===============================================================================
#
# UAS and ALS point cloud Z grid metrics
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 Aug 2020
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
# Computes point cloud Z metrics on a 20 m for UAS and ALS data. Computes Mark 
# Tukman's ladder fuel metric, Filippelli et al. (2019) density metric and lidR
# standard Z metrics.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# uas_las_file = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from UAS data height normalized using
#   ALS dtm.
# als_las_file = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from ALS data height normalized using
#   ALS dtm.
# res = grid resolution (meters)
# out_folder = output folder for grid metric rasters
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
# ===============================================================================

library(sp)
library(raster)
library(lidR)
library(tidyverse)
library(glue)

set_lidr_threads(0)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

uas_las_file <- 'D:/data/las/uas/processed/ppwd_uas_z{z}_f2_hnorm-als.las'

als_las_file <- 'data/las/als/ppwd_als_z{z}_hnorm-als.las'

res = 10

out_folder <- 'data/grid_metrics/rasters'

# ====== Function for ladder fuel, density and standard metrics calculation =====

z_metrics <- function(z) {
  
  n_0to4 = sum(z < 4 & z > 0)
  n_1to4 = sum(z < 4 & z > 1)
  ladder_fuel = n_1to4/n_0to4
  
  n_z = length(z)
  d00 = sum(z < 5 & z > 2)/n_z
  d01 = sum(z < 10 & z > 5)/n_z
  d02 = sum(z < 15 & z > 10)/n_z
  d03 = sum(z < 15 & z > 50)/n_z
  
  ladder_metrics = list(
    n_0to4 = n_0to4,
    n_1to4 = n_1to4,
    ladder_fuel = ladder_fuel,
    d00 = d00,
    d01 = d01,
    d02 = d02,
    d03 = d03,
    n = n_z
  )
  
  return(c(ladder_metrics, stdmetrics_z(z)))
  
}

# ================== Compute grid metrics for UAS and ALS data ================== 

for (z in zone) {
  
  message('Computing grid metrics for zone ', z)
  
  uas_grid <- glue(uas_las_file) %>%
    readLAS(select = '') %>%
    grid_metrics(~z_metrics(Z), res = res)
  
  writeRaster(
    x = uas_grid,
    filename = glue('{out_folder}/ppwd_uas_z{z}_f2_hnorm-als_grid-metrics_{res}m-grid'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)
  
  als_grid <- glue(als_las_file) %>%
    readLAS(select = '') %>%
    grid_metrics(~z_metrics(Z), res = uas_grid$n_0to4)
  
  writeRaster(
    x = als_grid,
    filename = glue('{out_folder}/ppwd_als_z{z}_hnorm-als_grid-metrics_{res}m-grid'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)

}

# ===============================================================================
