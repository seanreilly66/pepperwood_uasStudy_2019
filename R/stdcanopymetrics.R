# ===============================================================================
#
# LidR standard canopy metrics comparison
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 4 June 2020
# Last commit: 29 July 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Compares standard canopy metrics as implemented in lidR between UAS and ALS point
# clouds. Also computes ladder fuel metric using Tukman ALs derived method.
# 
# Requires height normalized UAS point clouds. Includes function to normalize ALS
# point cloud using a user supplied DTM
# 
# ===============================================================================
# 
# User inputs:
#
# zone = Zone number from user prompt. Used in completing filenames for las_file and 
#    standard_dtm files
# uas_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from UAS data
# als_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from ALS data
# als_dtm = .tif file name (.las faster operation) skeleton (containing {z} zone 
#   glue placeholder) for dtm raster from ALS data
# grid_ras_out = output file location for grid metric rastersrt5fd
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
# Plenty. Still under development.
#
# ===============================================================================

lib = NULL

suppressPackageStartupMessages(library(sp, lib.loc = lib))
suppressPackageStartupMessages(library(raster, lib.loc = lib))
suppressPackageStartupMessages(library(lidR, lib.loc = lib))
suppressPackageStartupMessages(library(tidyverse, lib.loc = lib))
suppressPackageStartupMessages(library(glue, lib.loc = lib))

rm(lib)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

uas_las <- 'data/las/uas/ppwd_uas_z{z}_f2_hnorm.las'
als_las <- 'data/las/als/ppwd_als_z{z}.las'
als_dtm <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'

grid_ras_out <- 'data/grid_metrics/rasters'

# ============= ALS height normalization and noise filter function ============== 

ctg_normnoise = function(las_file, noise_sensitivity, dtm) {
  
  ctg <- readLAScatalog(las_file)
  
  opt_chunk_size(ctg) <- 250
  opt_chunk_buffer(ctg) <- 30
  opt_output_files(ctg) <- glue('{tempfile()}_{{ID}}')
  opt_select(ctg) <- ''
  
  normnoise <- function(cluster, sensitivity, dtm) {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    
    las <- lasnormalize(las, dtm, na.rm = TRUE)
    
    p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
    las <- lasmergespatial(las, p95, "p95")
    las <- lasfilter(las, Z < p95*sensitivity)
    las$p95 <- NULL
    
    las <- lasfilter(las, buffer == 0)
    return(las)
  }
  
  ctg_to_las <- function(cluster) {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(las)
  }
  
  ctg <- catalog_apply(ctg = ctg, 
                       FUN = normnoise, 
                       sensitivity = noise_sensitivity,
                       dtm = dtm)
  
  ctg <- readLAScatalog(unlist(ctg))
  
  opt_chunk_buffer(ctg) = 0
  opt_select(ctg) <- ''

  las <- catalog_sapply(ctg = ctg, 
                        FUN = ctg_to_las)
  
  return(las)
}

# ================== Ladder fuel and standard metrics function ================== 

z_metrics <- function(z) {
  
  n_0to4 = sum(z < 4 & z > 0)
  n_1to4 = sum(z < 4 & z > 1)
  
  ladder_fuel = n_1to4/n_0to4
  
  ladder_metrics = list(
    n_0to4 = n_0to4,
    n_1to4 = n_1to4,
    ladder_fuel = ladder_fuel
  )
  
  return(c(ladder_metrics, stdmetrics_z(z)))
}

# ================================ Grid metrics =================================

for (z in zone) {
  
  message('Computing grid metrics zone ', z)
  
  als_grid <- glue(als_las) %>%
    ctg_normnoise(
      noise_sensitivity = 1.2,
      dtm = raster(glue(als_dtm))) %>%
    grid_metrics(~z_metrics(Z))
  
  writeRaster(
    x = als_grid,
    filename = glue('{grid_ras_out}/ppwd_als_z{z}_gridmetrics'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)
  
  uas_grid <- glue(uas_las) %>%
    readLAS(select = '') %>%
    grid_metrics(~z_metrics(Z))
  
  writeRaster(
    x = uas_grid,
    filename = glue('{grid_ras_out}/ppwd_uas_z{z}_f2_gridmetrics'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)
  
  
  grid_dif = uas_grid - als_grid
  
  writeRaster(
    x = grid_dif,
    filename = glue('{grid_ras_out}/ppwd_dif_z{z}_gridmetrics'),
    datatype='FLT4S',
    format="GTiff",
    bylayer = TRUE,
    suffix = 'names',
    overwrite=TRUE)

}