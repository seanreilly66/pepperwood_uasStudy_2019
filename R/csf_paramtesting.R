# ===============================================================================
#
# Cloth simulation parameter testing for optimized dtm generation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 Feb 2020
# Last commit: 2 June 2019
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Produces sequence of DTMs from a point cloud using Cloth Simulation Filter (CSF)
# ground (CSF) finding algorithm from Zhang et al. (2016) with a supplemental NDVI 
# reclassification filter in order to test parameter effects on DTM accuracy. CSF 
# algorithm iterated over a given range of values for one parameter while all others
# held constant and a DTM is generated from the resulting ground points. NDVI filter 
# applied to ground points prior to DTM generation to reclassify ground points 
# identified by CSF as ground but with NDVI value above a given threshold in order 
# to remove misclassified dense vegetated areas.
# 
# Error is computed in comparison to a user-supplied DTM standard (e.g. derived from 
# ALS data) within a user-supplied buffered ROI. CSF and other ground finding algorithms
# are prone to edge effects so error metrics most accurately portray algorithm 
# performance away from the perimeter. Error is the cell level difference between the 
# provided DTM standard and the DTM produced by each set of CSF parameters. Cell 
# level error for each CSF parameter set summarized as RMSE, mean error, error standard 
# deviation, % area with <2m error, and error quantiles. Also computes RMSE by
# vegetation class as given by user-supplied vegetation rasters. See further details
# below about vegetation raster class requirements.
#
# Script optimized to be run repeatedly on Linux cluster for different flight zones.
# Therefore, currently prompts user to enter a zone number and this is used in 
# selecting all other files.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Zone number from user prompt. Used in completing filenames for las_file and 
#    standard_dtm files
# round = Processing round number, for repeating the processing process
# las_file = .las or .laz point cloud file name (.las faster operation)
# {parameter}_value = Constant value for each parameter. This value will be used
#     when iterating over the other parameter's ranges. See below for parameter names
# {parameter}_testrange = Vector of values for each parameter to be iterated over
# dtm_res = DTM resolution (square meters)
# standard_dtm = .tif filename of dtm to be used as comparison benchmark (e.g. from ALS)
# roi = Buffered ROI shapefile to use as focal point for error calculations. Must have
#     Zone attribute if contains multiple polygons.
# veg_class = .tif filename of raster containing vegetation classification data
# output = filename to write output csv to
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
# sp, raster, lazyeval, rgeos, rlas, RCSF, rgdal, lidR, vctrs, backports, withr, 
# rstudioapi, tidyverse, glue
# 
# ===============================================================================

lib = 'r_lib' 
# lib = NULL # for local use

suppressPackageStartupMessages(library(sp, lib.loc = lib))
suppressPackageStartupMessages(library(raster, lib.loc = lib))
suppressPackageStartupMessages(library(lazyeval, lib.loc = lib))
suppressPackageStartupMessages(library(rlas, lib.loc = lib))
suppressPackageStartupMessages(library(RCSF, lib.loc = lib))
suppressPackageStartupMessages(library(lidR, lib.loc = lib))
suppressPackageStartupMessages(library(rgdal, lib.loc = lib))
suppressPackageStartupMessages(library(rgeos, lib.loc = lib))
suppressPackageStartupMessages(library(vctrs, lib.loc = lib))
suppressPackageStartupMessages(library(backports, lib.loc = lib))
suppressPackageStartupMessages(library(withr, lib.loc = lib))
suppressPackageStartupMessages(library(rstudioapi, lib.loc = lib))
suppressPackageStartupMessages(library(tidyverse, lib.loc = lib))
suppressPackageStartupMessages(library(glue, lib.loc = lib))

# ================================= User inputs =================================

zone <- as.integer(readline('zone number: '))
round <- 1

las_file <- glue('data/las/uas/ppwd_uas_z{zone}_f2_reg2als.las')

cr_value <- 0.5
ct_value <- 0.5
r_value <- 1
ts_value <- 0.65
n_value <- 0

cr_testrange <- seq(0.1, 2.5, 0.1)
ct_testrange <- seq(0.1, 2.5, 0.1)
r_testrange <- 1:3
ts_testrange <- seq(0.1, 2.5,0.1)
n_testrange <- seq(0, 1, 0.1)

dtm_res <- 1

standard_dtm <- glue('data/dtm/als/ppwd_als_z{zone}_dtm.tif')

roi <- 'data/site_data/zone_shp/ppwd_zones_50mBuffer.shp'

veg_class <- glue('data/site_data/veg_class/zone/ppwd_vegclass_z{zone}.tif')

output <- glue('data/dtm/csf_paramtesting/ppwd_csf_paramtesting_rnd{round}_z{zone}.csv')
     
# =============== CSF ground finding and DTM generation function ================
  
csf_dtmgen <- function(las, cr, ct, r, ts, n, dtm_res) {
  
  las_ground <- lasground(las, 
                          algorithm = csf(
                            class_threshold = ct,
                            cloth_resolution = cr,
                            rigidness = r,
                            time_step = ts,
                            iterations = 500L,
                            sloop_smooth = FALSE),
                          last_returns = FALSE) %>%
    lasfilterground()
  
  if (n != 0) {
    las_ground <- lasfilter(las_ground, NDVI <= n)
  }

  if (length(las_ground$X) < 1000) {
    return(NULL)
  } 
  
  dtm <- suppressWarnings(
    grid_terrain(las = las_ground,
                 res = dtm_res,
                 algorithm = tin())) # Suppress degenerated ground point warning. No effect on ouput.

  return(dtm)
  
}
  
# ========================== Error calculation function =========================
  
error_calc <- function(dtm, test_param, standard_dtm, veg_class, roi) {
  
  if (is.null(dtm)) {return(NULL)}
  
  dtm_error <- data.frame(
    'testing_parameter' = test_param,
    'cloth_resolution' = cr, 
    'class_threshold' = ct, 
    'cloth_rigidness' = r, 
    'time_step' = ts, 
    'ndvi_filter' = n, 
    'dtm_cells' = ncell(dtm))
  
  dtm <- dtm %>%
    mask(roi) %>%
    raster::trim() %>%
    resample(standard_dtm)
    
  dtm <- dtm - standard_dtm %>%
    raster::trim()
  
  if (ncell(dtm) < 500) {return(NULL)}
  
  dtm <- dtm %>% 
    as.data.frame(na.rm = TRUE, xy = TRUE) %>%
    left_join(veg_class, 
              by = c('x', 'y')) %>%
    dplyr::select('err' = 3, 'veg_class' = 4)
  
  veg_error <- dtm %>%
    filter(veg_class == floor(veg_class)) %>%
    filter(veg_class != 1 & veg_class != 4) %>%
    group_by(veg_class) %>%
    summarise(
      'ss_veg' = sum(err^2, na.rm = TRUE),
      'n_veg' = n()
    ) 
  
  veg_type <- unique(veg_error$veg_class)
  
  veg_error <- veg_error %>%
    add_column(ss_veg_title = glue('ss_veg_class{veg_type}'), .before = 'ss_veg') %>%
    add_column(n_veg_title = glue('n_veg_class{veg_type}'), .before = 'n_veg')
  
  ss_veg <- veg_error %>%
    dplyr::select('ss_veg_title', 'ss_veg') %>%
    column_to_rownames('ss_veg_title') %>%
    t() %>%
    as.data.frame()
  
  n_veg <- veg_error %>%
    dplyr::select('n_veg_title', 'n_veg') %>%
    column_to_rownames('n_veg_title') %>%
    t() %>%
    as.data.frame()
    
  error_summary <- dtm %>%
    summarise(
      'rmse' = sqrt(mean(err^2, na.rm = TRUE)),
      'prcnt_LT2' = (sum(err < 2) / nrow(dtm)),
      'avg' = mean(err, na.rm = TRUE),
      'stdev' = sd(err, na.rm = TRUE),
      'range' = max(err, na.rm = TRUE) - min(err, na.rm = TRUE),
      'IQR' = IQR(err),
      'ss_zone' = sum(err^2, na.rm = TRUE),
      'n_zone' = n()) %>%
    add_column(dtm_error, .before = 'rmse') %>%
    add_column(ss_veg, n_veg)

  return(error_summary)
  
}
  
# =============================== Load site data ================================
  
las <- readLAS(las_file, select = '3') # 3 = NDVI extrabytes slot

roi <- rgdal::readOGR(roi, verbose = FALSE) %>%
    subset(Zone == zone|zone == zone) %>%
    spTransform(projection(las)) # Suppress invalid crs warning from intersect function

standard_dtm <- raster(standard_dtm) %>%
  mask(roi) %>%
  raster::trim()

veg_class <- raster(veg_class) %>%
  resample(standard_dtm, method = 'ngb') %>%
  mask(roi) %>%
  raster::trim() %>%
  as.data.frame(na.rm = TRUE, xy = TRUE)

# ============================ CSF parameter testing ============================

csf_param_error <- data.frame(matrix(nrow = 0, ncol = 27)) %>%
  mutate(1, factor())

colnames(csf_param_error) <- c('testing_parameter', 'cloth_resolution', 'class_threshold', 
  'cloth_rigidness', 'time_step', 'ndvi_filter', 'dtm_cells', 'rmse', 'prcnt_LT2', 'avg', 
  'stdev', 'range', 'IQR', 'ss_zone', 'n_zone', 'ss_veg_class2', 'ss_veg_class3', 'ss_veg_class5',
  'ss_veg_class6', 'ss_veg_class7', 'ss_veg_class8', 'n_veg_class2', 'n_veg_class3', 
  'n_veg_class5', 'n_veg_class6', 'n_veg_class7', 'n_veg_class8')

cr = cr_value
ct = ct_value
r = r_value
ts = ts_value
n = n_value


test_param = 'cloth_resolution'
message('Processing ', test_param)

for(cr in cr_testrange) {
  message('\n', test_param, ': ', cr)
  dtm <- csf_dtmgen(las, cr, ct, r, ts, n, dtm_res)
  error_summary <- error_calc(dtm, test_param, standard_dtm, veg_class, roi)
  if (!is.null(error_summary)) {
    csf_param_error <- add_row(csf_param_error, error_summary)
  }
}

cr <- cr_value


test_param = 'class_threshold'
message('Processing ', test_param)

for(ct in ct_testrange) {
  message('\n', test_param, ': ', ct)
  dtm <- csf_dtmgen(las, cr, ct, r, ts, n, dtm_res)
  error_summary <- error_calc(dtm, test_param, standard_dtm, veg_class, roi)
  if (!is.null(error_summary)) {
    csf_param_error <- add_row(csf_param_error, error_summary)
  }
}

ct <- ct_value


test_param = 'cloth_rigidness'
message('Processing ', test_param)

for(r in r_testrange) {
  message('\n', test_param, ': ', r)
  dtm <- csf_dtmgen(las, cr, ct, r, ts, n, dtm_res)
  error_summary <- error_calc(dtm, test_param, standard_dtm, veg_class, roi)
  if (!is.null(error_summary)) {
    csf_param_error <- add_row(csf_param_error, error_summary)
  }
}

r <- r_value


test_param = 'time_step'
message('Processing ', test_param)

for(ts in ts_testrange) {
  message('\n', test_param, ': ', ts)
  dtm <- csf_dtmgen(las, cr, ct, r, ts, n, dtm_res)
  error_summary <- error_calc(dtm, test_param, standard_dtm, veg_class, roi)
  if (!is.null(error_summary)) {
    csf_param_error <- add_row(csf_param_error, error_summary)
  }
}

ts <- ts_value

# ============================= NDVI filter testing =============================

test_param = 'ndvi_filter'
message('Processing ', test_param)

las_ground <- lasground(las, 
                        algorithm = csf(
                          class_threshold = ct,
                          cloth_resolution = cr,
                          rigidness = r,
                          time_step = ts,
                          iterations = 500L,
                          sloop_smooth = FALSE),
                        last_returns = FALSE) %>%
  lasfilterground()

for(n in n_testrange) {
  message('\n', test_param, ': ', n)
  
  if (n == 0) {
    las_ndvi <- las_ground
  } else {
    las_ndvi <- lasfilter(las_ground, NDVI <= n)
  }
  
  if (length(las_ndvi$X) < 1000) {
    next
  } 
  
  dtm <- suppressWarnings(
    grid_terrain(las = las_ndvi,
                 res = dtm_res,
                 algorithm = tin())) # Suppress degenerated ground point warning. No effect on ouput.
  
  error_summary <- error_calc(dtm, test_param, standard_dtm, veg_class, roi)
  if (!is.null(error_summary)) {
    csf_param_error <- add_row(csf_param_error, error_summary)
  }
}

# ============================== Write CSV to file ==============================

write.csv(csf_param_error, output, row.names = FALSE)
  
  
  

