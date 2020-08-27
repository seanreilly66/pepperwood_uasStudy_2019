# ===============================================================================
#
# One vs two flight direction uas dap dtm error comparison
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 24 June 2020
# Last commit: 29 July 2020
#
# Status: Functional, see known problems for details
#
# This script created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Compares dtm generation results for uas point clouds generated from either one
# or two flight directions. Comparison aims to determine the resolution lost from
# only using one flight direction in order to inform future field campaigns. Uses
# optimized CSF ground finding algorithm parameters as determined using the script
# csf_paramtesting.R. Error is computed for each generated dtm in comparison to an 
# ALS-derived dtm benchmark.
# 
# ===============================================================================
# 
# User inputs:
#
# zone = Zone numbers. Set to loop over sequence of zones
# f2_dtm_file = .tif file name skeleton (containing {zone} glue placeholder) for 
#   dtm generated from two flight data using optimized csf parameters
# f1_las_file = .las or .laz file name (.las faster operation) skeleton (containing
#   {zone} glue placeholder) for point cloud of single-flight data registered to ALS
# dtm_res = DTM resolution (square meters)
# {parameter}_value = Optimized value for each CSF parameter, as determined with 
#   csf_paramtesting.R
# roi = Buffered ROI shapefile to use as focal point for error calculations. Must have
#     Zone number attribute.
# bnchmrk_dtm = .tif filename skeleton (containing {zone} glue placeholder) for dtm 
#   to be used as comparison benchmark (e.g. from ALS)
# veg_class = .tif filename skeleton (containing {z} zone glue placeholder) for 
#   vegetation classification data raster
# f1_dtm_output = output file location for one flight dtm
# error_ras_output = output file location for error rasters
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
# Vegetation classification raster:
#
# Vegetation classification raster should contain integer values 1 through 8 
# cooresponding to the following vegetation classes:
#   1 = Human (farm, building, vineyard, etc.)
#   2 = Grassland
#   3 = Shrubland
#   4 = Water
#   5 = Wet herbaceous
#   6 = Deciduous broadleaf
#   7 = Evergreen broadleaf
#   8 = Conifer
#
# Human and water classes are filtered out to restrict to vegetation
# 
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, rgdal, lidR, tidyverse, glue
#
# Additional packages for linux cluster use:
#
# lazyeval, rlas, RCSF, vctrs, backports, withr, rstudioapi
# 
# ===============================================================================
# 
# Known problems:
#
# If a one flight dtm for a zone is not generated due to insufficient ground points
# (null returned), all subsequent steps will fail. Need to implement a check for 
# file existence.
#
# Could be optimized to run as one for loop, rather than reading and writing to file
# but intermediate outputs are useful for other purposes and raster read time is
# short compared to overall function processing time.
#
# ===============================================================================

lib = NULL # for local use

suppressPackageStartupMessages(library(lidR, lib.loc = lib))
suppressPackageStartupMessages(library(rgdal, lib.loc = lib))
suppressPackageStartupMessages(library(tidyverse, lib.loc = lib))
suppressPackageStartupMessages(library(glue, lib.loc = lib))

rm('lib')

# ================================= User inputs =================================

zone <- c(2:4,6:13)

f2_dtm_file <- 'data/dtm/uas/ppwd_uas_z{z}_f2_dtm.tif'
  
f1_las_file <-  'data/las/uas/ppwd_uas_z{z}_f1_reg2als.las'

dtm_res <- 1
cr_value <- 0.68
ct_value <- 0.18
r_value <- 3
ts_value <- 0.65
n_value <- 0.53

roi <- 'data/site_data/zone_shp/ppwd_zones_50mBuffer.shp'

bnchmrk_dtm <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'
veg_class <- 'data/site_data/veg_class/zone/ppwd_vegclass_z{z}.tif'

f1_dtm_output <- 'data/dtm/uas'
error_ras_output <- 'data/dtm/error_rasters'

# ============================ One flight dtm raster =============================

message('Generating one-flight dtm rasters')

for (z in zone) {
  
  message('Processing zone ', z)
  
  f1_dtm <- glue(f1_las_file) %>%
    readLAS(select = '3') %>%  # Read only ndvi from extra attributes
    lasground(
      algorithm = csf(
        class_threshold = ct_value,
        cloth_resolution = cr_value,
        rigidness = r_value,
        time_step = ts_value,
        iterations = 1000L,
        sloop_smooth = FALSE),
      last_returns = FALSE) %>%
    lasfilterground() %>%
    lasfilter(NDVI <= n_value) # NDVI threshold filter
  
  if (length(f1_dtm$X) < 1000) {next}
  
  f1_dtm <- suppressWarnings( # Suppress degenerated ground point warning. No effect on ouput.
    grid_terrain(
      las = f1_dtm,
      res = dtm_res,
      algorithm = tin())) 
  
  writeRaster(
    x = f1_dtm,
    filename = glue('{f1_dtm_output}/ppwd_uas_z{z}_f1_dtm'),
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
}

rm('f1_dtm', 'f1_las_file', 'cr_value', 'ct_value', 'r_value', 'ts_value', 'n_value', 'dtm_res', 'z')

# ==================== dtm error in comparison to benchmark =====================

message('Generating error rasters')

roi <- readOGR(roi, verbose = FALSE) %>%
  spTransform("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

for (z in zone) {
  
  message('Processing zone ', z)
  
  z_bnchmrk <- glue(bnchmrk_dtm) %>%
    raster() %>%
    mask(subset(roi, Zone == z)) %>%
    raster::trim()
  
  
  f1_dtm <- glue('{f1_dtm_output}/ppwd_uas_z{z}_f1_dtm.tif') %>%
    raster() %>%
    mask(subset(roi, Zone == z)) %>%
    raster::trim() %>%
    resample(z_bnchmrk)
  
  f1_error <- f1_dtm - z_bnchmrk %>%
    raster::trim()
  
  writeRaster(
    x = f1_error,
    filename = glue('{error_ras_output}/ppwd_z{z}_f1_dtm_error'),
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
  
  f2_dtm <- glue(f2_dtm_file) %>%
    raster() %>%
    mask(subset(roi, Zone == z)) %>%
    raster::trim() %>%
    resample(z_bnchmrk)
  
  f2_error <- f2_dtm - z_bnchmrk %>%
    raster::trim()
  
  writeRaster(
    x = f2_error,
    filename = glue('{error_ras_output}/ppwd_z{z}_f2_dtm_error'),
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
}

rm('f1_dtm', 'f1_error', 'f2_dtm', 'f2_error', 'roi', 'z_bnchmrk', 'bnchmrk_dtm', 
   'f1_dtm_output', 'f2_dtm_file', 'z')

# ========================= Compute error differences ===========================

message('Computing flight error differences')

dtm_error <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(dtm_error) <- c('zone', 'x', 'y', 'f1_error', 'f2_error', 'error_dif', 'veg_class')

for (z in zone) {
  
  message('Processing zone ', z)
  
  f1_error <- glue('{error_ras_output}/ppwd_z{z}_f1_dtm_error.tif') %>%
    raster()
  
  f2_error <- glue('{error_ras_output}/ppwd_z{z}_f2_dtm_error.tif') %>%
    raster()
  
  error_dif <- abs(f1_error) - abs(f2_error)
  
  writeRaster(
    x = error_dif,
    filename = glue('{error_ras_output}/ppwd_z{z}_dtm_f1vf2_errordif'),
    datatype='FLT4S',
    format="GTiff",
    overwrite=TRUE)
  
  z_veg <- glue(veg_class) %>%
    raster() %>%
    resample(f2_error)
  
  z_error <- stack(f1_error, f2_error, error_dif, z_veg) %>%
    as.data.frame(na.rm = TRUE, xy = TRUE) %>%
    add_column(zone = z, .before = 1)
  
  colnames(z_error) <- c('zone', 'x', 'y', 'f1_error', 'f2_error', 'error_dif', 'veg_class')
  
  dtm_error <- add_row(dtm_error, z_error)

}


write.csv(dtm_error, glue('{error_ras_output}/ppwd_uas_f1vf2_errordif.csv'))

rm('error_ras_output', 'z_error', 'z_veg', 'error_dif', 'f1_error', 'f2_error', 
   'z', 'veg_class', 'zone')

# ============================ Summarize error =============================

full <- dtm_error %>%
  summarise(
    'f1_rmse' = sqrt(mean(f1_error^2, na.rm = TRUE)),
    'f2_rmse' = sqrt(mean(f2_error^2, na.rm = TRUE)),
    'f1_mean' = mean(f1_error, na.rm = TRUE),
    'f2_mean' = mean(f2_error, na.rm = TRUE),
    'f1_stdev' = sd(f1_error, na.rm = TRUE),
    'f2_stdev' = sd(f2_error, na.rm = TRUE),
    'f1_range' = max(f1_error, na.rm = TRUE) - min(f1_error, na.rm = TRUE),
    'f2_range' = max(f2_error, na.rm = TRUE) - min(f2_error, na.rm = TRUE),
    'f1_IQR' = IQR(f1_error),
    'f2_IQR' = IQR(f2_error),
    'mean_dif' = mean(error_dif, na.rm = TRUE)) %>%
  add_column(group = 'All Pepperwood', .before = 1) %>%
  mutate_at('group', factor)

zone <- dtm_error %>%
  group_by(zone) %>%
  summarise(
    'f1_rmse' = sqrt(mean(f1_error^2, na.rm = TRUE)),
    'f2_rmse' = sqrt(mean(f2_error^2, na.rm = TRUE)),
    'f1_mean' = mean(f1_error, na.rm = TRUE),
    'f2_mean' = mean(f2_error, na.rm = TRUE),
    'f1_stdev' = sd(f1_error, na.rm = TRUE),
    'f2_stdev' = sd(f2_error, na.rm = TRUE),
    'f1_range' = max(f1_error, na.rm = TRUE) - min(f1_error, na.rm = TRUE),
    'f2_range' = max(f2_error, na.rm = TRUE) - min(f2_error, na.rm = TRUE),
    'f1_IQR' = IQR(f1_error),
    'f2_IQR' = IQR(f2_error),
    'mean_dif' = mean(error_dif, na.rm = TRUE)) %>%
  rename(zone, group = zone) %>%
  mutate_at('group', factor) %>%
  mutate(group = fct_recode(group,
                            'Zone 2' = '2',
                            'Zone 3' = '3',
                            'Zone 4' = '4',
                            'Zone 6' = '6',
                            'Zone 7' = '7',
                            'Zone 8' = '8',
                            'Zone 9' = '9',
                            'Zone 10' = '10',
                            'Zone 11' = '11',
                            'Zone 12' = '12',
                            'Zone 13' = '13'))
  
veg <- dtm_error %>%
  filter(veg_class == floor(veg_class)) %>%
  filter(veg_class != 1 & veg_class != 4) %>%
  group_by(veg_class) %>%
  summarise(
    'f1_rmse' = sqrt(mean(f1_error^2, na.rm = TRUE)),
    'f2_rmse' = sqrt(mean(f2_error^2, na.rm = TRUE)),
    'f1_mean' = mean(f1_error, na.rm = TRUE),
    'f2_mean' = mean(f2_error, na.rm = TRUE),
    'f1_stdev' = sd(f1_error, na.rm = TRUE),
    'f2_stdev' = sd(f2_error, na.rm = TRUE),
    'f1_range' = max(f1_error, na.rm = TRUE) - min(f1_error, na.rm = TRUE),
    'f2_range' = max(f2_error, na.rm = TRUE) - min(f2_error, na.rm = TRUE),
    'f1_IQR' = IQR(f1_error),
    'f2_IQR' = IQR(f2_error),
    'mean_dif' = mean(error_dif, na.rm = TRUE)) %>%
  rename(group = veg_class) %>%
  mutate_at('group', factor) %>%
  mutate(group = fct_recode(group,
                            'Grassland' = '2',
                            'Shrubland' = '3',
                            'Wet Herbaceous' = '5',
                            'Deciduous Broadleaf' = '6',
                            'Evergreen Broadleaf' = '7',
                            'Conifer' = '8'
                            ))

summary <- full %>%
  add_row(zone) %>%
  add_row(veg) %>%
  mutate_at(2:12, round, digits = 2)

write.csv(summary, glue('{error_ras_output}/ppwd_uas_f1vf2_errordif_summary.csv'))
