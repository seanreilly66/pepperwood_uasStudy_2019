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
library(parallel)

# ================================= User inputs ================================

uas_file <- 'data/las/uas/raw_pix4d/ppwd_uas_z{z}_f2_pix4d.las'
spec_file <-
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

grnd_las_output <-
  'data/icp_free_analysis/ground_points/ppwd_uas_z{z}_f2_noicp_grndpts.las'

grnd_df_output <- 'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts.csv'

hnorm_las_output <- 'data/las/uas/ppwd_uas_z{z}_f2_noicp_hnrom-dtm.las'

# =========================== Isolate ground points ============================

# grnd <- function(z, zone_shp, uas, ct_value, cr_value, r_value, ts_value, 
#                  n_value, spectral_file, dtm, output) {
#   
#   library(lidR)
#   library(tidyverse)
#   library(glue)
#   library(sf)
#   
#   zone_shp <- read_sf(zone_shp) %>%
#     st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')) %>%
#     filter(Zone == z)
#   
#   if (z %in% 6:7) {
#     las <- glue(uas, z = 67) %>%
#       readLAS(select = '') 
#     red <- glue(spectral_file, band = 'red', z = 67) %>%
#       raster()
#     nir <- glue(spectral_file, band = 'nir', z = 67) %>%
#       raster()
#   } else {
#     las <- glue(uas, z = z) %>%
#       readLAS(select = '') 
#     red <- glue(spectral_file, band = 'red') %>%
#       raster()
#     nir <- glue(spectral_file, band = 'nir') %>%
#       raster()
#   }
# 
#   las <- las %>%
#     clip_roi(zone_shp)
#   
#   ndvi <- (nir - red) / (nir + red)
#   
#   las <- las %>%
#     filter_duplicates() %>%
#     merge_spatial(source = ndvi,
#                   attribute = 'NDVI')
#   
#   las <- classify_ground(
#     las = las,
#     algorithm = csf(
#       class_threshold = ct_value,
#       cloth_resolution = cr_value,
#       rigidness = r_value,
#       time_step = ts_value,
#       iterations = 500L,
#       sloop_smooth = FALSE
#     ),
#     last_returns = FALSE
#   ) %>%
#     filter_ground() %>%
#     filter_poi(NDVI <= n_value)
#   
#   writeLAS(las, glue(output, z = z))
#   
# }
# 
# cl <- makeCluster(6)
# 
# z = zone
# 
# parLapply(
#   cl,
#   z,
#   grnd,
#   zone_shp = zone_shp_file,
#   spectral_file = spec_file,
#   uas = uas_file,
#   dtm = als_dtm, 
#   r_value <- 3,
#   cr_value <- 0.45,
#   ts_value <- 0.58,
#   ct_value <- 0.01,
#   n_value <- 0.55,
#   output = grnd_las_output
# )
# 
# stopCluster(cl)


zone_shp <- read_sf(zone_shp_file) %>%
  st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))

for (z in zone) {
  message('Processing zone ', z)
  
  if (z %in% 6:7) {
    las <- glue(uas_file, z = 67) %>%
      readLAS(select = '') %>%
      clip_roi(zone_shp %>%
                 filter(Zone == z))
    
    red <- glue(spec_file, band = 'red', z = 67) %>%
      raster()
    nir <- glue(spec_file, band = 'nir', z = 67) %>%
      raster()
    
  } else {
    las <- glue(uas_file) %>%
      readLAS(select = '') %>%
      clip_roi(zone_shp %>%
                 filter(Zone == z))
    
    red <- glue(spec_file, band = 'red') %>%
      raster()
    nir <- glue(spec_file, band = 'nir') %>%
      raster()
    
  }
  
  ndvi <- (nir - red) / (nir + red)
  
  las <- las %>%
    filter_duplicates() %>%
    merge_spatial(source = ndvi,
                  attribute = 'NDVI')
  
  
  plot(decimate_points(las, random(1)), color = 'NDVI')
  rgl::rgl.snapshot(glue('qc/', 
                         str_extract(grnd_las_output, 'ppwd.+(?=.las)'), 
                         '_spectral.png'))
  rgl::rgl.close()
  
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
  
  plot(decimate_points(las, random(1)), color = 'NDVI')
  rgl::rgl.snapshot(glue('qc/', 
                         str_extract(grnd_las_output, 'ppwd.+(?=.las)'), 
                         '_grnd_pts.png'))
  rgl::rgl.close()
  
  writeLAS(las, glue(grnd_las_output))
  
}
plot(decimate_points(las, random(1)), color = 'NDVI')
rm(ct_value, cr_value, r_value, ts_value, n_value, spectral_file, 
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
  
  las <- glue(grnd_las_output) %>%
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

write_csv(grnd_pts, grnd_df_output)

rm(buffered_shp_file, grnd_las_output, las, dtm, buffered_shp)
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

if (!exists('grnd_pts')) {
  grnd_pts <- read_csv(grnd_df_output)
}

fig = ggplot(data = grnd_pts %>%
         sample_frac(0.1) %>%
         mutate(zone = as.factor(zone)),
       mapping = aes(
         x = uas_z,
         y = dtm_z
       )) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x, color = 'firebrick') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  stat_cor(label.y = 420) +
  stat_regline_equation(label.y = 365) +#365) +
  labs(
    x = 'UAS ground points elevation (m)',
    y = 'ALS DTM elevation (m)'
  ) +
  facet_wrap(~zone, 
             ncol = 3) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

ggsave(filename = 'figures/no_icp_regression.png', 
       plot = fig,
       width = 6.5, height = 7.5, units = 'in', dpi = 700)

rm(fig)

# ====================== Height normalize by zone ======================

zone_shp <- read_sf(zone_shp_file) %>%
  st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs'))

if (!exists('grnd_pts')) {
  grnd_pts <- read_csv(grnd_df_output)
}

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

  als_las <- 'data/las/als/ppwd_als_z{z}_hnorm-als.las' %>%
    glue() %>%
    readLAS(select = '') %>%
    decimate_points(random(1))
  
  dtm <- als_dtm %>%
    glue() %>%
    raster()
  
  model <- lm(uas_z ~ dtm_z, 
              data = grnd_pts %>%
                filter(zone == z))
  
  dtm <- dtm + model$coefficients[1]
  
  las <- normalize_height(las, dtm, na.rm = TRUE)

  x = plot(las %>%
             decimate_points(random(1)))
  plot(als_las, add = x, colorPalette = 'white')
  rgl::rgl.snapshot(glue(
    'qc/',
    str_extract(grnd_las_output, 'ppwd.+(?=.las)'),
    '_hnorm.png'
  ))
  rgl::rgl.close()
  
  writeLAS(las, glue(hnorm_las_output))
  
}



# dtm_norm <- function(z, uas, dtm, grnddf, zone_shp, output) {
#   
#   library(lidR)
#   library(tidyverse)
#   library(glue)
#   library(sf)
#   
#   zone_shp <- 'data/site_data/zone_shp/ppwd_zones.shp' %>%
#     read_sf() %>%
#     st_transform(crs('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')) %>%
#     filter(Zone == z)
#   
#   grnd_pts <- 'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts.csv' %>%
#     read_csv() %>%
#     filter(zone == z)
#   
#   las <- 'data/las/uas/raw_pix4d/ppwd_uas_z{z}_f2_pix4d.las' %>%
#     glue(z = z) %>%
#     readLAS(select = '') %>%
#     clip_roi(zone_shp)
#   
#   als_las <- 'data/las/als/ppwd_als_z{z}_hnorm-als.las' %>%
#     glue(z = z) %>%
#     readLAS(select = '') %>%
#     decimate_points(random(1))
#   
#   dtm <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif' %>%
#     glue(z = z) %>%
#     raster()
#   
#   # ==============================================================================
#   # ============================== Analysis ======================================
#   # ==============================================================================
#   
#   model <- lm(uas_z ~ dtm_z, data = grnd_pts)
#   
#   # ==============================================================================
#   
#   dtm <- dtm + model$coefficients[1]
#   
#   uas_las <- normalize_height(las, dtm, na.rm = TRUE)
#   
#   # ==============================================================================
#   # ================================ Plot results ================================ 
#   # ==============================================================================
#   
#   x = plot(uas_las  %>%
#              decimate_points(random(1)))
#   plot(als_las, add = x, colorPalette = 'white')
#   rgl::rgl.snapshot(glue('qc/', 
#                          str_extract(output, 'ppwd.+(?=.las)'), 
#                          '_hnorm.png'))
#   rgl::rgl.close()
#   
#   writeLAS(las, glue(output))
#   
# }
# 
# cl <- makeCluster(11)
# 
# z = c(2:4,8:13)
# 
# parLapply(
#   cl,
#   z,
#   dtm_norm,
#   uas = uas_file,
#   dtm = als_dtm,
#   grnddf = grnd_df_output,
#   zone_shp = zone_shp_file,
#   output = hnorm_las_output
# )
# 
# stopCluster(cl)
# 
