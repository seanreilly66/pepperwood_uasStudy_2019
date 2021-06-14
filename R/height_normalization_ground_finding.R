# This is an excerpt from the longer code. This is just the part that does the
# ground finding. It reads in a raw pix4d uas las file ('uas_las_file'), 
# spectral files ('uas_spec_file'), and a shape file which represents the zone 
# boundary ('buffered_shp_file'). In this case, I am using one that is buffered
# out 50m. For the spectral file, it specifically needs the nir and red bands
# to compute NDVI which is used to filter ground points. 
# 
# I have the code setup to run in parallel so it can process multiple zones at
# once. It then extracts the ground points and generates a dataframe of uas and
# als corresponding ground point elevations. For this purpose, it reads in an
# ALS dtm file ('als_dtm_file'). 
# 
# I have a fair bit of processing power so depending on the machine you are
# using, you will likely have to either reduce the number of cores (in line 52).
# If that is still throwing an error, you can change the code from running in
# parallel to runing in sequence by replacing %dopar% in line 61 with just %do%.
# 
# Like in my other scripts, I have all of my uas files named with only the zone
# or band names differentiating them. That way, I can loop through them by using
# a glue {} placeholder (e.g., {z}, {band}). Depending on how you have your files
# named, you will probably have to change the structure of the name and the loop.
# The foreach package uses the first argument (z = zone in this case) as the
# looping variable. You can also give it a vector of file names, you'll just need
# to change the inside of the loop where the glue lines appear accordingly.

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

buffered_shp_file <- 'data/site_data/zone_shp/ppwd_zones_50m-buffer.shp'

als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'

grnd_las_output <-
  'data/icp_free_analysis/ground_points/ppwd_uas_z{z}_f2_noicp_grndpts.las'

grnd_df_output <- 'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts.csv'



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
