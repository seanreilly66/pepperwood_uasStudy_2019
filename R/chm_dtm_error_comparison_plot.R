# ==============================================================================
#
# CHM vs DTM absolute error comparison plot
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 April 2020
# Last commit: 14 Sept 2020
#
# Status: Functional
#
# This file created as part of 2019 Pepperwood UAS study
#
# ==============================================================================
#
# Description:
#
# Generates scatterplot of chm error versus dtm error using compiled chm dataset
# from chm_data-compile.R
#
# ==============================================================================
# 
# User inputs:
#
# input = input chm dataset from chm_data-compile.R
# output = output figure filename, including file type extension
#
# ==============================================================================
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
# ==============================================================================
#
# RBR classification scheme:
#
# 1 = Unburned
# 2 = Low burn severity
# 3 = Medium burn severity
# 4 = High burn severity
# 
# ==============================================================================
#
# Topography classification raster:
#   1 = Valley
#   2 = Slope
#   3 = Flat
#   4 = Slope
#   5 = Slope
#   6 = Ridge
# This classification scheme combines all slope types into one group
#
# ==============================================================================
# 
# Package dependences: 
#
# tidyverse, glue
# 
# ==============================================================================
#
# Known problems:
#
# none
#
# ==============================================================================

# =============================== Load libraries =============================== 

library(tidyverse)
library(glue)

# ================================= User inputs ================================

input <- 'data/chm/ppwd_chm_compiled-data.csv'
output <- 'figures/fig6_chm_vs_dtm_error.png'

# =========================== Read in data from file =========================== 

compiled_data <- data.table::fread(input)

# ======================== Data preparation for analysis =======================

plot_data <- compiled_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(
    chm_error = abs(uas_chm_hnorm.als - uas_chm_hnorm.uas),
    dtm_error = abs(uas_dtm - als_dtm))

# ============================== Set ggplot theme =============================== 

theme_set(
  theme(text = element_text(family = 'serif', face = 'plain'),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        line = element_line(size = 1),
        axis.line = element_line(),
        panel.background = element_rect(color = 'white'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        legend.spacing = unit(0, "cm"),
        legend.margin = margin(0,5,0,5)
  )
)

# =========== Correlation and scatter plot of chm error vs dtm error =========== 

r <- cor(
  x = plot_data$dtm_error,
  y = plot_data$chm_error,
  method = 'pearson') %>%
  as.numeric() %>%
  round(2)


fig <- ggplot(
  data = plot_data %>%
  slice_sample(prop = 0.3),
  mapping = aes(
    x = dtm_error,
    y = chm_error)) +
  geom_point(
    size = 0.6,
    alpha = 0.2) +
  labs(
    x = 'UAS-SfM DTM absolute error (m)',
    y = 'UAS-SfM CHM absolute error (m)') + 
  ylim(0,30) +
  xlim(0,40) +
  geom_text(
    aes(x = 35, 
        y = 1, 
        label = glue('r = +{r}')),
    family = 'serif', 
    fontface = 'plain',
    size = 5.5)

# fig

ggsave(
  filename = output,
  width = 6, 
  height = 4.5, 
  units = 'in', 
  dpi = 700)

# ===============================================================================