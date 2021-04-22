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
# Package dependencies:
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(sf)
library(ggpubr)
library(doParallel)
library(lemon)

# ================================= User inputs ================================

zone <- c(2:4, 6:13)

uas_las_file <- 'data/las/uas/raw_pix4d/ppwd_uas_z{z}_f2_pix4d.las'
uas_spec_file <-
  'data/las/uas/raw_pix4d/spectral/ppwd_uas_z{z}_f2_pix4d_{band}.tif'

zone_shp_file <- 'data/site_data/zone_shp/ppwd_zones.shp'
buffered_shp_file <- 'data/site_data/zone_shp/ppwd_zones_50m-buffer.shp'

als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'

grnd_las_output <-
  'data/icp_free_analysis/ground_points/ppwd_uas_z{z}_f2_noicp_grndpts.las'

grnd_df_output <- 'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts.csv'

hnorm_grnd_df_output <- 
  'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts_hnorm_fulleq.csv'

hnorm_las_output <- 'data/las/uas/ppwd_uas_z{z}_f2_noicp_hnorm-dtm_fulleq.las'



# ==============================================================================
# ========================== Classify ground points ============================
# ==============================================================================

# -------------------------- Setup cluster processing --------------------------

cl <- makeCluster(11)
registerDoParallel(cl)

grnd_pts <- foreach (
  z = zone,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  
  # ------------------ Prep LAS + merge NDVI and ALS DTM -----------------------
  
  zone_shp <- read_sf(zone_shp_file) %>%
    st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))
  
  if (z %in% 6:7) {
    las <- glue(uas_las_file, z = 67) %>%
      readLAS(select = '') 
    red <- glue(uas_spec_file, band = 'red', z = 67) %>%
      raster()
    nir <- glue(uas_spec_file, band = 'nir', z = 67) %>%
      raster()
  } else {
    las <- glue(uas_las_file) %>%
      readLAS(select = '') 
    red <- glue(uas_spec_file, band = 'red') %>%
      raster()
    nir <- glue(uas_spec_file, band = 'nir') %>%
      raster()
  }
  
  ndvi <- (nir - red) / (nir + red)
  
  las <- las %>%
    filter_duplicates() %>%
    clip_roi(zone_shp %>%
               filter(Zone == z)) 
  
  las <- las %>%
    merge_spatial(source = ndvi,
                  attribute = 'NDVI') %>%
    add_lasattribute(name = 'NDVI', desc = 'NDVI')

  # -------------------------- Ground classification --------------------------- 
  
  las <- classify_ground(
    las = las,
    algorithm = csf(
      class_threshold = 0.01,
      cloth_resolution = 0.45,
      rigidness = 3,
      time_step = 0.58,
      iterations = 500L,
      sloop_smooth = FALSE
    ),
    last_returns = FALSE
  ) 
  
  las@data <- las@data %>%
    mutate(Classification = replace(Classification, NDVI > 0.55, 1L))

  writeLAS(las, glue(grnd_las_output))

  # -------------- Isolate ground points and corresponding DTM Z --------------- 
  
  buffered_shp <- read_sf(buffered_shp_file) %>%
    st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))
  
  las <- las %>%
    filter_ground() %>%
    clip_roi(buffered_shp %>%
               filter(Zone == z))

  dtm <- glue(als_dtm_file) %>%
    raster()
  
  las <- las %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z')

  las <- las@data %>%
    rename(uas_z = Z) %>%
    select(uas_z, dtm_z) %>%
    add_column(zone = z)

}

write_csv(grnd_pts, grnd_df_output)

stopCluster(cl)



# ==============================================================================
# =============== Plot uas ground points to als dtm relationship =============== 
# ==============================================================================

# --------------------- Load ground points data if missing --------------------- 

if (!exists('grnd_pts')) {
  grnd_pts <- read_csv(grnd_df_output) 
}

# ------------------------------ Set ggplot theme ------------------------------ 

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

# ------------------------------- Generate plot -------------------------------- 

# fig = ggplot(data = grnd_pts %>%
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
#   stat_regline_equation(label.y = 365) +
#   labs(
#     x = 'UAS ground points elevation (m)',
#     y = 'ALS DTM elevation (m)'
#   ) +
#   facet_rep_wrap( ~ zone,
#                   ncol = 4) +
#   theme(
#     strip.text.x = element_text(size = 13, hjust = 0),
#     strip.background = element_blank(),
#     axis.line = element_line(),
#     plot.caption = element_text(size = 14, hjust = 0)
#   )
# 
# ggsave(filename = 'figures/no_icp_grndpts_regression.png', 
#        plot = fig,
#        width = 8.5, height = 7.5, units = 'in', dpi = 700)
# 
# rm(fig)



# ==============================================================================
# =========================== Height normalization =============================
# ==============================================================================

# --------------------- Load ground points data if missing --------------------- 

if (!exists('grnd_pts')) {
  grnd_pts <- read_csv(grnd_df_output) 
}

# -------------------------- Setup cluster processing -------------------------- 

cl <- makeCluster(4)
registerDoParallel(cl)

hnorm_grnd_pts <- foreach (
  z = zone,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  
  # ------------- Correct for offset between ground points and DTM ------------- 
  
  las <- glue(grnd_las_output) %>%
    readLAS(select = 'c')
  
  model <- lm(dtm_z ~ uas_z, 
              data = grnd_pts %>%
                filter(zone == z))
  
  las@data <- las@data %>%
    mutate(Z = Z*model$coefficients[2] + model$coefficients[1])
  
  # las@data <- las@data %>%
  #   mutate(Z = Z + model$coefficients[1])
  
  # ---------- Extract ground points and dtm Z for offset confirmation --------- 
  
  grnd <- las %>%
    filter_ground() 
  
  dtm <- glue(als_dtm_file) %>%
    raster()
  
  grnd <- grnd %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z')
  
  # ---------------------- DTM based height normalization ---------------------- 
  
  las <- normalize_height(las, dtm, na.rm = TRUE)
  
  writeLAS(las, glue(hnorm_las_output))
  
  # ----------------------- Output ground points dataset ----------------------- 

  grnd <- grnd@data %>%
    rename(uas_z = Z) %>%
    select(uas_z, dtm_z) %>%
    add_column(zone = z)
  
}

write_csv(hnorm_grnd_pts, hnorm_grnd_df_output)

stopCluster(cl)


# ==============================================================================
# =============== Plot uas ground points to als dtm relationship =============== 
# ==============================================================================

# --------------------- Load ground points data if missing --------------------- 

if (!exists('hnorm_grnd_pts')) {
  hnorm_grnd_pts <- read_csv(hnorm_grnd_df_output) 
}

# ------------------------------ Set ggplot theme ------------------------------ 

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

# ------------------------------- Generate plot -------------------------------- 

# fig = ggplot(data = hnorm_grnd_pts %>%
#                sample_frac(1) %>%
#                mutate(zone = as.factor(zone)),
#              mapping = aes(
#                x = uas_z,
#                y = dtm_z
#              )) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y~x, color = 'firebrick') +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   stat_cor(label.y = 420) +
#   stat_regline_equation(label.y = 365) +#365) +
#   labs(
#     x = 'UAS ground points elevation (m)',
#     y = 'ALS DTM elevation (m)',
#     caption = 'Offset corrected using only intercept of regression eq'
#   ) +
#   facet_rep_wrap( ~ zone,
#                   ncol = 4) +
#   theme(
#     strip.text.x = element_text(size = 13, hjust = 0),
#     strip.background = element_blank(),
#     axis.line = element_line(),
#     plot.caption = element_text(size = 14, hjust = 0)
#   )
# 
# ggsave(filename = 'figures/no_icp_hnorm_grndpts_regression_interceptonly.png', 
#        plot = fig,
#        width = 8.5, height = 7.5, units = 'in', dpi = 700)
# 
# rm(fig)
  
# ====================== Height normalize by zone ======================

# zone_shp <- read_sf(zone_shp_file) %>%
#   st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))
# 
# if (!exists('grnd_pts')) {
#   grnd_pts <- read_csv(grnd_df_output)
# }
# 
# for (z in zone) {
#   
#   message('Processing zone ', z)
#   
#   if (z %in% 6:7) {
#     las <- glue(uas_file, z = 67) %>%
#       readLAS(select = '') %>%
#       clip_roi(zone_shp %>%
#                  filter(Zone == z))
#   } else {
#     las <- glue(uas_file) %>%
#       readLAS(select = '') %>%
#       clip_roi(zone_shp %>%
#                  filter(Zone == z))
#   }
# 
  als_las <- 'data/las/als/ppwd_als_z{z}_hnorm-als.las' %>%
    glue() %>%
    readLAS(select = '') %>%
    decimate_points(random(1))
#   
#   dtm <- als_dtm %>%
#     glue() %>%
#     raster()
#   
#   model <- lm(uas_z ~ dtm_z, 
#               data = grnd_pts %>%
#                 filter(zone == z))
#   
#   dtm <- dtm + model$coefficients[1]
# 
#   las <- normalize_height(las, dtm, na.rm = TRUE)
# 
  x = plot(las %>%
             decimate_points(random(10)))
  plot(als_las, add = x, colorPalette = 'grey')
#   rgl::rgl.snapshot(glue(
#     'qc/',
#     str_extract(grnd_las_output, 'ppwd.+(?=.las)'),
#     '_hnorm.png'
#   ))
#   rgl::rgl.close()
#   
#   writeLAS(las, glue(hnorm_las_output))
#   
# }


