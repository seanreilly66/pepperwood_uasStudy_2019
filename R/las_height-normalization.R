# ===============================================================================
#
# UAS point cloud height normalization
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 Aug 2020
# Last commit: 16 Aug 2020
#
# Status: Functional
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Height normalizes UAS point cloud data using both UAS and ALS dtm data
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# uas_las_file = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from UAS data
# als_las_file = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from ALS data
# als_dtm_file = .tif file name skeleton (containing {z} zone glue placeholder) for 
#   dtm raster from ALS data
# uas_dtm_file = .tif file name skeleton (containing {z} zone glue placeholder) for 
#   dtm raster from UAS data
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
# Needs user input for output file location and format (currently hard coded)
#
# ===============================================================================

lib = NULL

library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(lidR, lib.loc = lib)
library(glue, lib.loc = lib)

set_lidr_threads(0)

rm(lib)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

uas_las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_reg2als.las'
als_las_file <- 'data/las/als/ppwd_als_z{z}.las'
als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'
uas_dtm_file <- 'data/dtm/uas/ppwd_uas_z{z}_f2_dtm.tif'

# ============= LAS height normalization and noise filter function ============== 

normnoise <- function(cluster, sensitivity, dtm) {
  
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  
  las <- normalize_height(las, dtm, na.rm = TRUE, add_lasattribute = TRUE)
  
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  
  las <- filter_poi(las, buffer == 0)
  return(las)
  
}

ctg_to_las <- function(cluster) {
  
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  return(las)
  
}

ctg_normnoise <- function(dtm, ctg_las_file) {

  ctg <- glue(ctg_las_file)
  ctg <- readLAScatalog(ctg)

  opt_chunk_size(ctg) <- 150
  opt_chunk_buffer(ctg) <- 30
  opt_output_files(ctg) <- glue('{tempfile()}_{{ID}}')
  opt_chunk_alignment(ctg) <- c(ctg$Min.X,ctg$Min.Y)

  ctg <- catalog_apply(
    ctg = ctg,
    FUN = normnoise,
    sensitivity = 1.2,
    dtm = dtm)

  ctg <- readLAScatalog(unlist(ctg))

  opt_chunk_buffer(ctg) = 0

  ctg <- catalog_sapply(
    ctg = ctg,
    FUN = ctg_to_las)
  
  return(ctg)
}

# ======================== UAS height normalized to ALS ========================= 

for (z in zone) {
  
  als_dtm <- raster(glue(als_dtm_file))
  
  uas_hnorm2als <- ctg_normnoise(
    dtm = als_dtm, 
    ctg_las_file = uas_las_file)
  
  writeLAS(uas_hnorm2als, glue('data/las/uas/ppwd_uas_z{z}_f2_hnorm-als.las'))
  
}

rm(z, als_dtm, uas_hnorm2als)

# # ======================== ALS height normalized to ALS ========================= 
# 
# for (z in zone) {
#   
#   als_dtm <- raster(glue(als_dtm_file))
#   
#   als_hnorm2als <- ctg_normnoise(
#     dtm = als_dtm, 
#     ctg_las_file = als_las_file)
#   
#   writeLAS(als_hnorm2als, glue('data/las/als/ppwd_als_z{z}_hnorm-als.las'))
#   
# }
# 
# rm(z, als_dtm, als_hnorm2als)
# 
# # ======================== UAS height normalized to UAS ========================= 
# zone = 8:13
# for (z in zone) {
#   
#   uas_dtm <- raster(glue(uas_dtm_file))
#   
#   uas_hnorm2uas <- ctg_normnoise(
#     dtm = uas_dtm, 
#     ctg_las_file = uas_las_file)
#   
#   writeLAS(uas_hnorm2uas, glue('D:/data/las/uas/processed/ppwd_uas_z{z}_f2_hnorm-uas.las'))
#   
# }

rm(z, als_dtm, uas_hnorm2uas)
