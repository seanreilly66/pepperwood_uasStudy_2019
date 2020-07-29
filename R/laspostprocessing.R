# ===============================================================================
#
# Las file post-processing for MicaSense RedEdge-MX
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 14 Nov 2019
# Last commit: 29 July 2020
#
# Status: Functional, see known problems for details
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Merges Pix4D las point cloud and multispectral data, reprojects data if given,
# and clips to given boundary. Spectral is converted to 16-Bit by rescaling by 
# 10000. Also computes NDVI and adds to las. Function loads all data from file, 
# user does not load into memory before calling function. Designed to work with 
# five bands of spectral data from a Micasence RedEdge sensor. Returns las file 
# with associated spectral data. Writes las to file if filename is given. Ignore
# warning about files having different attribute 0.
#
# This is a function only for use in other scripts.
#
# ===============================================================================
# 
# User inputs:
#
# las_file = raw pix4d .las or .laz point cloud file name (.las faster operation)
# spectral_stack = raw pix4d spectral reflectance .tif file names as character vector.
#     Expects five  bands from micasense rededge-mx
# boundary = ROI shapefile
# crs_projection = OPTIONAL. New projection, input must be PROJ.4 projection arguement.
# las_out = OPTIONAL. las output file name
#
# ===============================================================================
#
# Input file name requirements:
#
# Spectral .tif file names must include exact color band name: blue, green, red, rededge, nir. 
# Colors must be lower case and spelled out correctly. File path cannot contain
# any of these color names or will fail/produce erroneous result.
# Example: 'data/spectral/ppwd_uas_z5_blue.tif'
# 
# Pepperwood projection: 
#   +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# 
# ===============================================================================
# 
# Package dependences: 
#
# sp, raster, rgdal, lidR, tidyverse
#
# ===============================================================================
#
# Known problems:
#
# Shapefile intersection returns multiple polygons (not just the one of interest)
#
# ===============================================================================

suppressPackageStartupMessages(library('lidR'))
suppressPackageStartupMessages(library('tidyverse'))

laspostprocessing <- function(las_file, spectral_file, boundary, crs_projection = NULL, las_out = NULL) {

# ============= Convert spectral rasters to 16 bit and compute NDVI ============= 
  
  if (sum(str_detect(spectral_file, 'red(?!e)|green|blue|rededge|nir')) != 5) {
    stop('Incorrect spectral file band name. See documentation for details.', call. = FALSE)
  }
  
  raster_to_16bit <- function(spectral_file, band) {
    str_subset(spectral_file, band) %>%
      raster(bands = 1) %>%
      calc(function(x) {round(x * 10000)})
  }
  
  spectral <- stack(raster_to_16bit(spectral_file, 'red(?!e)'),
                    raster_to_16bit(spectral_file, 'green'),
                    raster_to_16bit(spectral_file, 'blue'),
                    raster_to_16bit(spectral_file, 'rededge'),
                    raster_to_16bit(spectral_file, 'nir')
                    )
  names(spectral) <- c('r','g','b','re','nir')
  
  spectral$ndvi <- (spectral$nir - spectral$r)/(spectral$nir + spectral$r)
  
# ====================== Add spectral bands to las catalog ======================
  
  las <- readLAScatalog(las_file)
  
  opt_chunk_size(las) <- 250
  opt_chunk_buffer(las) <- 0
  opt_select(las) <- ''
  opt_chunk_alignment(las) <- extent(las)[c(1,3)]
  opt_output_files(las) <- paste0(tempdir(),'\\addBands_{ID}')
  
  ctgmergespatial = function(cluster) {
    
    las <- readLAS(cluster) 
    if (is.empty(las)) {return(NULL)}
    
    las <- las %>%
      lasfilterduplicates() %>%
      lasmergespatial(stack(
        spectral$r,
        spectral$g,
        spectral$b
      )) %>%
      lasmergespatial(
        spectral$re, 
        attribute = 'RE'
      ) %>%
      lasmergespatial(
        spectral$nir,
        attribute = 'N'
      ) %>%
      lasmergespatial(
        spectral$ndvi,
        attribute = 'NDVI'
      ) %>%
      lasaddextrabytes(name = 'RE', desc = 'RedEdge') %>%
      lasaddextrabytes(name = 'N', desc = 'NIR') %>%
      lasaddextrabytes(name = 'NDVI', desc = 'NDVI')
    
    las <- las %>%
      lasfilterduplicates() %>%
      lasfilter(!(is.na(R)|is.na(G)|is.na(B)|is.na(RE)|is.na(N)|is.na(NDVI)))
    
    if (!is.null(crs_projection)) {
      las <- lastransform(las, crs_projection)
    }

    return(las)
  }

  options(future.globals.maxSize = 6291456000) # Increase to accodomate size of spectral stack 
  
  las <- catalog_apply(las, ctgmergespatial)
  
# =========================== Clip lascatalog to ROI ============================
    
  las <- readLAScatalog(unlist(las))
  
  opt_select(las) <- "0RGB"
  opt_chunk_buffer(las) <- 0
  
  shp <- rgdal::readOGR(boundary, verbose = FALSE) %>%
    spTransform(projection(las)) %>%
    raster::intersect(las) %>%
    buffer(width = 1, dissolve = TRUE)
  
  las <- suppressWarnings(lasclip(las, shp))
  
# ============================== Write las to file ==============================
  
  if (!is.null(las_out)) {
    writeLAS(las, las_out)
  }
  
  return(las)
}
