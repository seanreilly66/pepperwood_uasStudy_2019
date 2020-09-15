# ===============================================================================
#
# UAS final dtm generation results compilation and analysis
#
# ===============================================================================
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
# ===============================================================================
#
# Description:
#
# Generates large dataset containing uas dtm values, als dtm values, uas dtm error,
# vegetation classes, topography classes, and burn severities.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Vector of zone numbers
# chm_uas2uas_file <- .tif file name for chm generated from uas point cloud height
#   normalized to uas dtm (containing {z} zone glue placeholder)
# chm_uas2als_file <- .tif file name for chm generated from uas point cloud height
#   normalized to als dtm (containing {z} zone glue placeholder)
# chm_als2als_file <- .tif file name for chm generated from als point cloud height
#   normalized to als dtm (containing {z} zone glue placeholder)
# zone_buffered_shp_file = .shp file name (containing {z} zone glue placeholder)
#   for zone boundary buffered inward to remove edge artifacts
# als_dtm_file = .tif file name skeleton (containing {z} zone glue placeholder) for 
#   dtm raster from ALS data
# uas_dtm_file = .tif file name skeleton (containing {z} zone glue placeholder) for 
#   dtm raster from UAS data
# uas_dtm_smooth_file = .tif file name skeleton (containing {z} zone glue placeholder) for 
#   dtm raster from UAS data smoothed with two pass mean kernal filter
# veg_file = .tif file name (containing {z} zone glue placeholder) for recoded
#   vegetation classes (see below for details)
# rbr_file = .tif file name (containing {z} zone glue placeholder) for recoded
#   2017 Tubbs fire burn severity classes (see below for details)
# topo_file = .tif file name (containing {z} zone glue placeholder) for topography
#   classification data
# output = .csv file name for output dataframe
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
# ===============================================================================
#
# RBR classification scheme:
#
# 1 = Unburned
# 2 = Low burn severity
# 3 = Medium burn severity
# 4 = High burn severity
# 
# ===============================================================================
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
# Outputs hardcoded, documentation incomplete
#
# ===============================================================================

# =============================== Load libraries ================================ 

library(raster)
library(rgdal)
library(tidyverse)
library(glue)

# ================================= User inputs =================================

zone <-  c(2:4, 6:13)
zone_buffered_shp_file <- 'data/site_data/zone_shp/ppwd_zones_50m-buffer.shp'

chm_uas2uas_file <- 'data/chm/rasters/ppwd_uas_z{z}_f2_hnorm-uas_chm.tif'
chm_uas2als_file <- 'data/chm/rasters/ppwd_uas_z{z}_f2_hnorm-als_chm.tif'
chm_als2als_file <- 'data/chm/rasters/ppwd_als_z{z}_hnorm-als_chm.tif'

als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'
uas_dtm_file <- 'data/dtm/uas/ppwd_uas_z{z}_f2_dtm.tif'

veg_file <- 'data/site_data/veg_class/zone/ppwd_veg_z{z}.tif'
rbr_file <- 'data/site_data/tubbs17_rbr/zone/ppwd_tubbs17_rbr_z{z}.tif'
topo_file <- 'data/site_data/topography/zone/ppwd_topo_z{z}.tif'

output <- 'data/chm/ppwd_chm_compiled-data.csv'

# ============================= Compile zonal data ============================== 

col_names <- c('zone', 'uas_chm_hnorm.als', 'uas_chm_hnorm.uas', 'als_chm_hnorm.als', 
               'uas_dtm', 'als_dtm', 'veg_class', 'rbr_class', 'topo_class')

compiled_data <- matrix(nrow = 0, ncol = 9)
colnames(compiled_data) <- col_names
compiled_data <- as_tibble(compiled_data)

zone_buffer <- readOGR(zone_buffered_shp_file)

for (z in zone) {
  
  message('Processing zone: ', z)
  
  zone_roi <- subset(zone_buffer, Zone == z)
  
  uas_chm_hnorm.uas <- glue(chm_uas2uas_file) %>%
    raster() %>%
    mask(zone_roi) %>%
    raster::trim()
  
  uas_chm_hnorm.als <- glue(chm_uas2als_file) %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  als_chm_hnorm.als <- glue(chm_als2als_file) %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  uas_dtm <- glue(uas_dtm_file)  %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  als_dtm <- glue(als_dtm_file)  %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)

  veg_class <- glue(veg_file)  %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  rbr_class <- glue(rbr_file)  %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  topo_class <- glue(topo_file)  %>%
    raster() %>%
    resample(uas_chm_hnorm.uas)
  
  zone_data <- stack(uas_chm_hnorm.uas, uas_chm_hnorm.als, als_chm_hnorm.als, uas_dtm, als_dtm, veg_class, rbr_class, topo_class) %>%
    as.data.frame() %>%
    add_column(zone = z, .before = 1)
  
  colnames(zone_data) <- col_names
  
  zone_data <- zone_data %>%
    filter(!is.na(uas_dtm))
  
  compiled_data <- compiled_data %>%
    add_row(zone_data)
  
}

write_csv(compiled_data, output)

rm(zone_roi, zone_buffer, uas_chm_hnorm.uas, uas_chm_hnorm.als, als_chm_hnorm.als, 
   uas_dtm, als_dtm, veg_class, rbr_class, topo_class, zone_data, als_dtm_file, 
   col_names, rbr_file, topo_file, uas_dtm_file, veg_file, z, zone, zone_buffered_shp_file)

# ======================== Data preparation for analysis ======================== 

plot_data <- compiled_data %>%
  mutate(abs_uas_error = abs(uas_error)) %>%
  filter(
    veg_class%%1 < 0.05 | veg_class%%1 > 0.95,
    rbr_class%%1 < 0.05 | rbr_class%%1 > 0.95,
    veg_class %in% c(2,3,6,7,8)) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(veg_class = fct_recode(
    veg_class,
    'Grass' = '2',
    'Shrub' = '3',
    'Deciduous\nbroadleaf' = '6',
    'Evergreen\nbroadleaf' = '7',
    'Conifer' = '8')) %>%
  mutate(rbr_class = fct_recode(
    rbr_class,
    'None' = '1',
    'Low' = '2',
    'Med' = '3',
    'High' = '4'))

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

# ================ Boxplot of absolute error by vegetation type ================= 

outlier_label <- plot_data  %>%
  group_by(veg_class) %>%
  summarize(
    n_outlier = sum(abs_uas_error > (quantile(abs_uas_error, 0.75) + 1.5*IQR(abs_uas_error))),
    p_outlier = round(
      sum(abs_uas_error > (quantile(abs_uas_error, 0.75) + 1.5*IQR(abs_uas_error)))/n(),
      2),
    max = max(abs_uas_error, na.rm = TRUE) + 1
  )

fig <- ggplot(data = plot_data) +
  geom_boxplot(
    aes(
      x = veg_class,
      y = abs_uas_error,
      fill = veg_class)) +
  labs(
    x = NULL,
    y = 'UAS DTM absolute error (m)') +
  scale_fill_manual(values = c('#DDCC77', '#CC6677', '#88CCEE', '#332288', '#117733')) + 
  guides(fill = FALSE) +    
  geom_text(
    data = outlier_label,
    aes(x = veg_class, 
        y = max, 
        label = p_outlier),
    vjust=0,
    family = 'serif', 
    fontface = 'plain',
    size = 5) 


fig

ggsave(
  filename = 'figures/veg-class_vs_uas-dtm-abs-error.png',
  width = 6, 
  height = 4, 
  units = 'in', 
  dpi = 400)

# =============== Boxplot of absolute error by Tubbs burn severity ============== 

outlier_label <- plot_data  %>%
  group_by(rbr_class) %>%
  summarize(
    n_outlier = sum(abs_uas_error > (quantile(abs_uas_error, 0.75) + 1.5*IQR(abs_uas_error))),
    p_outlier = round(
      sum(abs_uas_error > (quantile(abs_uas_error, 0.75) + 1.5*IQR(abs_uas_error)))/n(),
      2),
    max = max(abs_uas_error, na.rm = TRUE) + 1
  )

fig <- ggplot(data = plot_data) +
  geom_hline(
    yintercept = 0,
    color = 'grey80',
    size = 1,
    linetype = 'dashed'
  ) +
  geom_boxplot(
    aes(
      x = rbr_class,
      y = abs_uas_error,
      fill = rbr_class)) +
  labs(
    x = 'RBR severity',
    y = 'UAS DTM absolute error (m)') +
  scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) + 
  guides(fill = FALSE) +    
  geom_text(
    data = outlier_label,
    aes(x = rbr_class, 
        y = max, 
        label = p_outlier),
    vjust = 0,
    family = 'serif', 
    fontface = 'plain',
    size = 5) 


fig

ggsave(
  filename = 'figures/tubbs-fire-rbr_vs_uas-dtm-abs-error.png',
  width = 6, 
  height = 4, 
  units = 'in', 
  dpi = 400)

# ===============================================================================