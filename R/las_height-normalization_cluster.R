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
# Height normalizes UAS point cloud data using both UAS and ALS dtm data.
# 
# Optimized for performance cluster application. Forgoes lascatalog in favor of 
# full zone processing.
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

zone = c(2:4, 6:13)

uas_las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_reg2als.las'
dtm_file <- 'data/dtm/{dtm_source}/ppwd_{dtm_source}_z{z}_dtm.tif'

als_las_file <- 'data/las/als/ppwd_als_z{z}.las'

uas_output <- 'data/las/uas/ppwd_uas_z{z}_f2_hnorm-{dtm_source}.las'
als_output <- 'data/las/als/ppwd_als_z{z}_hnorm-{dtm_source}.las'

# ============= LAS height normalization and noise filter function ============== 

normnoise <- function(las_file, sensitivity, dtm_file) {
  
  las <- readLAS(las_file)
  dtm <- raster(dtm_file)
  
  las <- normalize_height(las, dtm, na.rm = TRUE, add_lasattribute = TRUE)
  
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  
  return(las)
  
}

# ======================== UAS height normalized to ALS ========================= 

dtm_source = 'als'

message('Normalizing to ', dtm_source )

for (z in zone) {
  
  message('Processing zone: ', z)
  
  uas_hnorm2als <- normnoise(
    las_file = glue(uas_las_file),
    dtm = glue(dtm_file), 
    sensitivity = 1.2)
  
  writeLAS(uas_hnorm2als, glue(uas_output))
  
}

rm(z, uas_hnorm2als)

# ======================== UAS height normalized to UAS =========================

dtm_source = 'uas'

message('Normalizing to ', dtm_source )

for (z in zone) {
  
  message('Processing zone: ', z)
  
  uas_hnorm2als <- normnoise(
    las_file = glue(uas_las_file),
    dtm = glue(dtm_file), 
    sensitivity = 1.2)
  
  writeLAS(uas_hnorm2als, glue(uas_output))
  
}

rm(z, uas_hnorm2als)

# ======================== ALS height normalized to ALS ========================= 

dtm_source = 'als'

message('Normalizing to ', dtm_source )

for (z in zone) {
  
  message('Processing zone: ', z)
  
  als_hnorm2als <- normnoise(
    las_file = glue(als_las_file),
    dtm = glue(dtm_file), 
    sensitivity = 1.2)
  
  writeLAS(als_hnorm2als, glue(als_output))
  
}

rm(z, als_hnorm2als)

# ===============================================================================
