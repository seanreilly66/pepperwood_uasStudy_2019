# ===============================================================================
#
# LidR standard canopy metrics comparison
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 4 June 2020
# Last commit: 2 June 2019
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Compares standard canopy metrics as implemented in lidR between UAS and ALS point
# clouds. Comparison limited to areas of low burn severity during the 2017 Tubbs
# fire (Parks et al. 2014 RBR classes unchnaged and low). Some difference is expected
# due to canopy growth between sampling collection times (2013 and 2019). However, 
# no other major disturbance events besides the Tubbs fire occured between sampling 
# times so this variation should be minor and relatively consitent across the site.
# 
# Requires are height normalized UAS and ALS las point clouds
# 
# ===============================================================================
# 
# User inputs:
#
# zone = Zone number from user prompt. Used in completing filenames for las_file and 
#    standard_dtm files
# las_file = height normalized .las or .laz point cloud file name (.las faster operation)
# {parameter}_value = Constant value for each parameter. This value will be used
#     when iterating over the other parameter's ranges. See below for parameter names
# {parameter}_testrange = Vector of values for each parameter to be iterated over
# dtm_res = DTM resolution (square meters)
# standard_dtm = .tif filename of dtm to be used as comparison (e.g. from ALS)
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