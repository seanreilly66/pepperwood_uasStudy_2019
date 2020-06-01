# ===============================================================================
#
# Plot two las files to visualize their registration
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 20 Feb 2020
# Last commit: 1 June 2020
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Plots two las files on top of one another to visualize if registration was performed
# successfully. Las files are colored based on order they are given to plot function
# (blue for first file, red for second file). Can take in either las files loaded 
# into memory or las file names in which case it loads them from file but does not
# return them to memory. Randomly deprecates point cloud for to reduce plotting 
# memory restrictions. User can specify point density. LAS files must have same
# coordinate reference system.
#
# ===============================================================================
# 
# User inputs:
#
# las1 = las file in memory or .las or .laz file name (.las for faster operation)
# las2 = las file in memory or .las or .laz file name (.las for faster operation)
# density = Deprecated point cloud density
#
# ===============================================================================
# 
# Package dependences: 
#
# lidR, tidyverse
# 
# ===============================================================================

suppressPackageStartupMessages(library('lidR'))
suppressPackageStartupMessages(library('tidyverse'))

plot_lasreg <- function(las1, las2, density = 10) {
  
  if(is.character(las1)) {
    las1 <- readLAS(las1, select = '')
  }
  
  if (is.character(las2)) {
    las2 <-  readLAS(las2, select = '')
  }
  
  las1 <- las1 %>%
    lasfilterdecimate(random(density)) %>%
    lasadddata(rep(1, nrow(.@data)), name = 'file')
  
  las2 <- las2 %>%
    lasfilterdecimate(random(density)) %>%
    lasadddata(rep(2, nrow(.@data)), name = 'file')
  
  rbind(las1, las2) %>%
    plot(color = 'file')
  
}