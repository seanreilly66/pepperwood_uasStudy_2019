# ===============================================================================
#
# Cloth simulation parameter testing for optimized dtm generation - Round 1
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Feb 2020
# Last commit: 2 Sept 2020
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Produces final DTM from a point cloud using Cloth Simulation Filter (CSF)
# ground (CSF) finding algorithm from Zhang et al. (2016) with a supplemental NDVI 
# reclassification filter using set of optimal parameters as identified through two
# rounds of testing. DTM is smoothed with a two pass kernal smoothing filter to 
# remove local outliers.
# #
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers. Iterates over all zones
# las_file = .las or .laz point cloud file name (.las faster operation)
# {parameter}_value = Optimized value for each parameter. See below for parameter names
# dtm_res = DTM resolution (square meters)
# output = filename for output .tif destination
#
# ===============================================================================
#
# CSF parameters (see Zhang et al. 2016 for descriptions):
#
# cr = Cloth resolution
# ct = Class threshold
# r = Cloth rigidness
# ts = Time step
# n = NDVI threshold, restrict ground points used in DTM generation to only points
#     with NDVI below threshold. If NDVI threshold = 0, filter is not applied (off)
#
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, lazyeval, rgeos, rlas, RCSF, rgdal, lidR, vctrs, backports, withr, 
# rstudioapi, tidyverse, glue
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

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_reg2als.las'

r_value <- 3
cr_value <- 0.45
ts_value <- 0.58
ct_value <- 0.01
n_value <- 0.55

dtm_res <- 1

output <- 'data/dtm/uas/ppwd_uas_z{z}_f2_dtm'
     
# =============== CSF ground finding and DTM generation ================
  
for (z in zone) {
  
  message('Processing zone: ', z)
  
  las <- readLAS(glue(las_file), select = '3') # 3 = NDVI extrabytes slot
  
  las_ground <- classify_ground(
    las = las,
    algorithm = csf(
      class_threshold = ct_value,
      cloth_resolution = cr_value,
      rigidness = r_value,
      time_step = ts_value,
      iterations = 500L,
      sloop_smooth = FALSE),
      last_returns = FALSE)
  
  las_ground <- filter_ground(las_ground)
  
  las_ground <- filter_poi(las_ground, NDVI <= n_value)

  dtm <- suppressWarnings(
    grid_terrain(
      las = las_ground,
      res = dtm_res,
      algorithm = tin())) # Suppress degenerated ground point warning. No effect on ouput.
  
  writeRaster(
    dtm,
    filename = glue(output),
    format = 'GTiff',
    overwrite = TRUE
  )

  w <- matrix(1,3,3)
  dtm <- focal(dtm, w, fun = mean, na.rm = TRUE)
  dtm <- focal(dtm, w, fun = mean, na.rm = TRUE)
  
  writeRaster(
    dtm,
    filename = glue(output,'-smooth'),
    format = 'GTiff',
    overwrite = TRUE
  )
  
}
 
# ===============================================================================
