# ===============================================================================
#
# UAS and ALS point cloud P75 mean NDVI metric calculation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 Aug 2020
# Last commit: 14 Sept 2020
#
# Status: Complete
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Computes computes mean NDVI within +/- 1 m of 75th percentile Z height for 
# point cloud height normalized using ALS DTM
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
# Output hardcoded
#
# ===============================================================================

# =============================== Load libraries ================================ 

lib = 'r_lib' # for linux cluster use
# lib = NULL # for local use

suppressPackageStartupMessages(library(sp, lib.loc = lib))
suppressPackageStartupMessages(library(raster, lib.loc = lib))
suppressPackageStartupMessages(library(lazyeval, lib.loc = lib))
suppressPackageStartupMessages(library(rlas, lib.loc = lib))
suppressPackageStartupMessages(library(RCSF, lib.loc = lib))
suppressPackageStartupMessages(library(lidR, lib.loc = lib))
suppressPackageStartupMessages(library(rgdal, lib.loc = lib))
suppressPackageStartupMessages(library(rgeos, lib.loc = lib))
suppressPackageStartupMessages(library(glue, lib.loc = lib))

rm(lib)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

uas_las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_hnorm-als.las'

res = 20

out_folder <- 'data/grid_metrics/rasters'

# ==================== Function for ndvi metric calculation ===================== 

ndvi_metrics <- function(z, ndvi) {
  
  p75 = quantile(z, 0.75, na.rm = TRUE, names = FALSE)
  
  pos = which(z < (p75 + 1) & z > (p75 - 1))
  
  p75_ndvi = mean(ndvi[pos], na.rm = TRUE)
  
  ndvi_metrics = list(
    p75_ndvi = p75_ndvi
  )
  
  return(ndvi_metrics)
  
}

# ================== Compute grid metrics for UAS and ALS data ================== 

for (z in zone) {
  
  message('Computing grid metrics for zone ', z)
  
  uas_grid <- glue(uas_las_file) 
  uas_grid <- readLAS(uas_grid, select = '3')
  uas_grid <- grid_metrics(uas_grid, ~ndvi_metrics(Z, NDVI), res = res)
  
  writeRaster(
    x = uas_grid,
    filename = glue('{out_folder}/ppwd_uas_z{z}_f2_hnorm-als_grid-metrics_{res}m-grid'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)

}

# ===============================================================================
