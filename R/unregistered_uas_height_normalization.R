# ==============================================================================
#
# Unregistered height normalization using only ALS DTM (unregistered)
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 March 2021
# Last commit: 15 March 2021
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ==============================================================================
#
# Description:
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependences:
#
# ==============================================================================
#
# Known problems:
# 
# NO ZONE 7
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(sf)
library(ggpubr)

# ================================= User inputs ================================

uas_file <- 'data/las/uas/raw_pix4d/ppwd_uas_z{z}_f2_pix4d.las'
spectral_file <-
  'data/las/uas/raw_pix4d/spectral/ppwd_uas_z{z}_f2_pix4d_{band}.tif'
zone_shp_file <- 'data/site_data/zone_shp/ppwd_zones.shp'
buffered_shp_file <- 'data/site_data/zone_shp/ppwd_zones_50m-buffer.shp'

als_dtm <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'

zone <- c(2:4, 6:13)

r_value <- 3
cr_value <- 0.45
ts_value <- 0.58
ct_value <- 0.01
n_value <- 0.55

grndlas_output <-
  'data/icp_free_analysis/ground_points/ppwd_uas_z{z}_f2_noicp_grndpts.las'

grnddf_output <- 'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts.csv'

# =========================== Isolate ground points ============================

zone_shp <- read_sf(zone_shp_file) %>%
  st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))

memory.size(max = TRUE)

for (z in zone) {
  
  message('Processing zone ', z)
  
  if (z %in% 6:7) {
    las <- glue(uas_file, z = 67) %>%
      readLAS(select = '') %>%
      clip_roi(zone_shp %>%
                 filter(Zone == z))
  } else {
    las <- glue(uas_file) %>%
      readLAS(select = '') %>%
      clip_roi(zone_shp %>%
                 filter(Zone == z))
  }
  
  red <- glue(spectral_file, band = 'red') %>%
    raster()
  nir <- glue(spectral_file, band = 'nir') %>%
    raster()
  ndvi <- (nir - red) / (nir + red)
  
  las <- las %>%
    filter_duplicates() %>%
    merge_spatial(source = ndvi,
                  attribute = 'NDVI')
  
  las <- classify_ground(
    las = las,
    algorithm = csf(
      class_threshold = ct_value,
      cloth_resolution = cr_value,
      rigidness = r_value,
      time_step = ts_value,
      iterations = 500L,
      sloop_smooth = FALSE
    ),
    last_returns = FALSE
  ) %>%
    filter_ground() %>%
    filter_poi(NDVI <= n_value)
  
  writeLAS(las, glue(grndlas_output))
  
}

rm(zone_shp, ct_value, cr_value, r_value, ts_value, n_value, spectral_file, z, 
   zone_shp_file)

# ========================== Generate ground dataset ===========================

grnd_pts <- tibble(
  uas_z = as.numeric(),
  dtm_z = as.numeric(),
  zone = as.numeric()
)

buffered_shp <- read_sf(buffered_shp_file) %>%
  st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))

for (z in zone) {
  
  las <- glue(grndlas_output) %>%
    readLAS(select = '') %>%
    clip_roi(buffered_shp %>%
               filter(Zone == z))
  
  dtm <- glue(als_dtm) %>%
    raster()
  
  las <- las %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z') 
  
  las <- las@data %>%
    rename(uas_z = Z) %>%
    select(uas_z, dtm_z) %>%
    add_column(zone = z)
  
  grnd_pts <- grnd_pts %>%
    add_row(las)
  
}

write_csv(grnd_pts, grnddf_output)

rm(als_dtm, buffered_shp_file, uas_file, grndlas_output, las, dtm, buffered_shp)
gc()

# ================================ ggplot theme ================================

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0,0,0,0)
  )
)

# ====================== Plot uas to als dtm relationship ====================== 

# if (!exists('grnd_pts')) {
#   grnd_pts <- read_csv(grnddf_output)
# }
# 
# ggplot(data = grnd_pts %>%
#          sample_frac(1) %>%
#          mutate(zone = as.factor(zone)),
#        mapping = aes(
#          x = uas_z,
#          y = dtm_z
#        )) +
#   geom_point() + 
#   geom_smooth(method = 'lm', formula = y~x, color = 'firebrick') +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   stat_cor(label.y = 420) +
#   stat_regline_equation(label.y = 400) +
#   facet_wrap(~zone)

