# ==============================================================================
#
# Canopy height model error by vegetation type plot
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 26 April 2020
# Last commit: 7 Jan 2021
#
# Status: Functional
#
# This file created as part of 2019 Pepperwood UAS study
#
# ==============================================================================
#
# Description:
#
# Generates boxplot of chm error by vegetation type using compiled chm dataset
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
# tidyverse
#
# ==============================================================================
#
# Known problems:
#
# None
#
# ==============================================================================

# =============================== Load libraries ===============================

library(tidyverse)

# ================================= User inputs ================================

input <- 'data/chm/ppwd_chm_compiled-data.csv'
output <- 'figures/fig5_chm_error_by_veg_class.png'

# ====================== Read in data from file if needed ======================

compiled_data <- data.table::fread(input)

# ======================== Data preparation for analysis =======================

plot_data <- compiled_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  mutate(chm_error = abs(uas_chm_hnorm.als - uas_chm_hnorm.uas)) %>%
  filter(veg_class %% 1 < 0.05 | veg_class %% 1 > 0.95) %>%
  mutate_at('veg_class', round) %>%
  filter(veg_class %in% c(2, 3, 6, 7, 8)) %>%
  mutate_at(c('veg_class'), as_factor)


plot_data <- plot_data %>%
  add_row(plot_data %>%
            filter(veg_class %in% 6:8) %>%
            mutate(veg_class = 'All forests')) %>%
  mutate(
    veg_class = fct_recode(
      veg_class,
      'Grass' = '2',
      'Shrub' = '3',
      'Deciduous\nbroadleaf\nforest' = '6',
      'Evergreen\nbroadleaf\nforest' = '7',
      'Conifer\nforest' = '8'
    )
  ) %>%
  mutate(veg_class = fct_relevel(
    veg_class,
    c(
      'Grass',
      'Shrub',
      'Conifer\nforest',
      'Evergreen\nbroadleaf\nforest',
      'Deciduous\nbroadleaf\nforest',
      'All forests'
    )
  ))


# ============================== Set ggplot theme ==============================

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

# ================ Boxplot of absolute error by vegetation type ================

height_threshold <- 4

outlier_label <- plot_data  %>%
  group_by(veg_class) %>%
  summarize(
    n_outlier = sum(chm_error > (quantile(chm_error, 0.75) + 1.5*IQR(chm_error))),
    p_outlier = round(
      sum(chm_error > (quantile(chm_error, 0.75) + 1.5*IQR(chm_error)))/n(),
      2)*100,
    n_gtthreshold = sum(chm_error > height_threshold),
    p_gtthreshold = round(
      sum(chm_error > height_threshold)/n(),
      2)*100,
    max = max(chm_error, na.rm = TRUE) + 1
  )

fig <- ggplot(data = plot_data) +
  geom_hline(
    yintercept = height_threshold,
    color = 'grey',
    size = 1,
    linetype = 'dashed') +
  geom_boxplot(
    aes(
      x = veg_class,
      y = chm_error,
      fill = veg_class)) +
  labs(
    x = NULL,
    y = 'UAS-SfM CHM absolute error (m)') +
  scale_fill_manual(values = c('#DDCC77', '#CC6677', '#117733', '#332288', '#88CCEE', 'white')) + 
  guides(fill = FALSE) +    
  geom_text(
    data = outlier_label,
    aes(x = veg_class, 
        y = max, 
        label = glue('{p_gtthreshold}%')),
    vjust=0,
    family = 'serif', 
    fontface = 'plain',
    size = 5) +
  scale_y_continuous(
    limits = c(0,30),
    breaks = c(0, height_threshold, 10, 20, 30))

fig

ggsave(
  filename = output,
  width = 6.5, 
  height = 4.5, 
  units = 'in', 
  dpi = 700)
