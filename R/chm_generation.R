# ===============================================================================
#
# Canopy height model generation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 16 Aug 2020
# Last commit: 14 Sept 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Generates canopy height models for UAS and ALS height normalized data using
# the pitfree algorithm with default parameters. Performs a two pass kernel
# smoothing filter to remove outliers.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# las_file = .las or .laz file name (.las faster operation) skeleton for height 
#   normalized point cloud containing {pntcld_source}, {dtm_source}, and {z} zone
#   glue placeholders.
# chm_res = Canopy height model resolution (meters)
# output = Output file name containing {pntcld_source}, {dtm_source}, and {z} zone
#   glue placeholders
# 
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, lidR, glue
# 
# ===============================================================================
#
# Known problems:
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

uas_las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_hnorm-{dtm_source}.las'
als_las_file <- 'data/las/als/ppwd_uas_z{z}_hnorm-{dtm_source}.las'

chm_res <- 0.5

uas_output <- 'data/chm/ppwd_uas_z{z}_f2_hnorm-{dtm_source}_chm'
als_output <- 'data/chm/ppwd_als_z{z}_hnorm-{dtm_source}_chm'

# =========================== CHM generation function =========================== 

chm_gen <- function(las_file, res = chm_res) {
  
  las <- readLAS(las_file, select = ('r')) 
  
  chm <- grid_canopy(las, res, pitfree(subcircle = 0.15))
  
  ker = matrix(1,3,3)

  chm <- focal(chm, w = ker, fun = mean, na.rm = TRUE)
  chm <- focal(chm, w = ker, fun = mean, na.rm = TRUE)
  
  return(chm)

}

# ============ Generate CHMs from uas normalized to uas and als data ============ 

data_source <- c('uas', 'als')

for (dtm_source in data_source) {
  
  message('uas height normalized to ', dtm_source )
  
  for (z in zone) {
    message('Processing zone: ', z)
    
    chm <- chm_gen(las_file = glue(uas_las_file))

    writeRaster(
      x = chm,
      filename = glue(uas_output),
      datatype='FLT4S',
      format="GTiff",
      overwrite=TRUE)
  }
}

# ================ Generate CHMs from als normalized to als data ================ 

dtm_source = 'als'
  
message('als height normalized to als')

for (z in zone) {
  message('Processing zone: ', z)
  
  chm <- chm_gen(las_file = glue(als_las_file))
  
  writeRaster(
    x = chm,
    filename = glue(als_output),
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
}

# ===============================================================================