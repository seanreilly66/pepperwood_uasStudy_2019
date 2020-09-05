# ===============================================================================
#
# CSF parameter testing data compilation
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 4 Sept 2020
# Last commit: 4 Sept 2020
#
# Status: Complete
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Takes zonal error data from csf parameter testing and computes site-wide error
# values. Filters for only those parameter sets which cover at least 90% of the 
# zone area. Outputs compiled zonal data.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# rnd = CSF parameter testing round
# csf_result_file = .csv file name skeleton (including {z} zone and {rnd} round
#   glue placeholders) for CSF parameter results by zone from csf_parameter-testing_rnd#.R 
#   outputs
# compile_output = compiled zonal data output file name
# 
# ===============================================================================
# 
# Package dependences: 
#
# raster, tidyverse, glue, rgdal
# 
# ===============================================================================
#
# Known problems:
#
# ===============================================================================

library(tidyverse)
library(glue)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)
rnd <- 1
csf_result_file <- 'data/dtm/csf_parameter-testing/ppwd_csf-parameter-testing_rnd{rnd}_z{z}.csv'
compile_output <- 'data/dtm/csf_parameter-testing/ppwd_csf-parameter-testing_rnd{rnd}_z-compile.csv'
summary_output <- 'data/dtm/csf_parameter-testing/ppwd_csf-parameter-testing_rnd{rnd}_site-error.csv'

# ============ Filter for sufficient coverage and combine zonal data ============ 

csf_compile <- matrix(ncol = 28, nrow = 0)
colnames(csf_compile) <- c('zone', 'testing_parameter', 'cloth_resolution', 'class_threshold', 'cloth_rigidness', 
                           'time_step', 'ndvi_filter', 'dtm_cells', 'rmse', 'prcnt_LT2', 'avg', 'stdev', 
                           'range', 'IQR', 'ss_zone', 'n_zone', 'ss_veg_class2', 'ss_veg_class3', 'ss_veg_class5',
                           'ss_veg_class6', 'ss_veg_class7', 'ss_veg_class8', 'n_veg_class2', 'n_veg_class3',
                           'n_veg_class5', 'n_veg_class6', 'n_veg_class7', 'n_veg_class8')
csf_compile <- as_tibble(csf_compile) %>%
  mutate_all(as.numeric) %>%
  mutate_at('testing_parameter', as.character)

for (z in zone) {

  csf_result <- glue(csf_result_file) %>%
    read_csv() %>%
    filter(n_zone > 0.9*(max(n_zone, na.rm = TRUE))) %>%
    add_column('zone' = z, .before = 'testing_parameter')
  
  csf_compile <- csf_compile %>%
    add_row(csf_result)
  
}

write_csv(
  csf_compile,
  glue(compile_output)
)

# ======================= Compute pepperwood wide metrics ======================= 

csf_summary <- csf_compile %>%
  group_by(testing_parameter, cloth_resolution, class_threshold, cloth_rigidness, 
           time_step, ndvi_filter) %>%
  summarize(
    n = n(),
    site_rmse = sqrt(sum(ss_zone)/sum(n_zone)),
    grass_rmse = sqrt(sum(ss_veg_class2)/sum(n_veg_class2)),
    shrub_rmse = sqrt(sum(ss_veg_class3)/sum(n_veg_class3)),
    wetherb_rmse = sqrt(sum(ss_veg_class5)/sum(n_veg_class5)),
    decid_rmse = sqrt(sum(ss_veg_class6)/sum(n_veg_class6)),
    evrgrn_rmse = sqrt(sum(ss_veg_class7)/sum(n_veg_class7)),
    conifer_rmse = sqrt(sum(ss_veg_class8)/sum(n_veg_class8)),
  ) %>%
  filter(n == 11)

write_csv(
  csf_summary,
  glue(summary_output)
)

# ===============================================================================