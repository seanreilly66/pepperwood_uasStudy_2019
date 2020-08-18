# ===============================================================================
#
# Point cloud canopy penetration comparison
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 30 July 2020
# Last commit: 30 July 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Compares vertical point cloud structure between lidar and uas derived point 
# clouds. Comparison done for largest contiguous patch of each vegetation class
# as given in inputed vegetation raster. Area limited to regions with low burn 
# severity during the 2017 Tubbs fire.
#
# Normalizes ALS and UAS data with ALS dtm for comparison consistency. If UAS data
# already normalized, first unnormalizes then renormalizes using input dtm.
#
# ===============================================================================
# 
# User inputs:
#
# zone = Zone number from user prompt. Used in completing filenames for las_file and 
#    standard_dtm files
# uas_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from UAS data.
# als_las = .las or .laz file name (.las faster operation) skeleton (containing
#   {z} zone glue placeholder) for point cloud from ALS data
# als_dtm = .tif file name (.las faster operation) skeleton (containing {z} zone 
#   glue placeholder) for dtm raster from ALS data
# grid_ras_out = output file location for grid metric rastersrt5fd
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
# Package dependences: 
#
# sp, raster, lidR, tidyverse, glue
# 
# ===============================================================================
#
# Known problems:
#
# Plenty. Still under development.
#
# ===============================================================================

lib = NULL

suppressPackageStartupMessages(library(sp, lib.loc = lib))
suppressPackageStartupMessages(library(raster, lib.loc = lib))
suppressPackageStartupMessages(library(lidR, lib.loc = lib))
suppressPackageStartupMessages(library(tidyverse, lib.loc = lib))
suppressPackageStartupMessages(library(glue, lib.loc = lib))
library(sf)
library(rgeos)
library(rgdal)

rm(lib)

# ================================= User inputs =================================

zone <- c(2:4, 6:13)

uas_las_file <- 'data/las/uas/ppwd_uas_z{z}_f2_reg2als.las'
als_las_file <- 'data/las/als/ppwd_als_z{z}.las'
als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'

veg_file <- 'data/site_data/veg_class/zone/ppwd_vegclass_z{z}.tif'

rbr_file <- 'data/site_data/tubbs17_rbr/zone/ppwd_tubbs_rbr_z{z}.tif'

zone_file <- 'data/site_data/zone_shp/ppwd_zones.shp'

# ================ Identify largest contiguous unburned forest patches ===================

z <- zone[1]

veg_ras <- raster(glue(veg_file))
rbr_ras <- raster(glue(rbr_file))

for (z in zone[-1]) {
  veg_ras <- glue(veg_file) %>%
    raster() %>%
    merge(veg_ras)
  
  rbr_ras <- glue(rbr_file) %>%
    raster() %>%
    merge(rbr_ras)
}

rbr_ras <- resample(rbr_ras, veg_ras)

veg_ras <- veg_ras %>%
  mask(
    mask = veg_ras %in% 6:8,
    maskvalue = 0) %>%
  mask(
    mask = rbr_ras %in% 0:2,
    maskvalue = 0)

veg_poly <- rasterToPolygons(veg_ras, n = 4, na.rm = TRUE) %>%
  st_as_sf() %>%
  rename(veg_class = layer)

zone_poly <- read_sf(zone_file) %>%
  st_transform(crs = st_crs(veg_poly))

veg_poly <- st_intersection(veg_poly, zone_poly) %>%
  select(veg_class, Zone, geometry)

veg_zone <- veg_poly %>%
  as.data.frame() %>%
  filter(Zone %in% 2:13) %>%
  group_by(Zone) %>%
  summarize(
    n_decid = sum(veg_class == 6),
    n_evrgrn = sum(veg_class == 7),
    n_conifer = sum(veg_class == 8)
  )

z_decid <- veg_zone %>%
  filter(n_decid == max(n_decid)) %>%
  select(Zone) %>%
  as.integer()

z_evrgrn <- veg_zone %>%
  filter(n_evrgrn == max(n_evrgrn)) %>%
  select(Zone) %>%
  as.integer()

z_conifer <- veg_zone %>%
  filter(n_conifer == max(n_conifer)) %>%
  select(Zone) %>%
  as.integer()

roi_decid <- veg_poly %>%
  filter(Zone == z_decid) %>%
  filter(veg_class == 6) %>%
  as_Spatial() %>%
  gBuffer(width = 1) %>%
  aggregate() %>%
  disaggregate()
  
roi_decid$area <- area(roi_decid)

roi_decid <- subset(roi_decid, roi_decid$area == max(roi_decid$area))

roi_evrgrn <- veg_poly %>%
  filter(Zone == z_evrgrn) %>%
  filter(veg_class == 7) %>%
  as_Spatial() %>%
  gBuffer(width = 1) %>%
  aggregate() %>%
  disaggregate()

roi_evrgrn$area <- area(roi_evrgrn)

roi_evrgrn <- subset(roi_evrgrn, roi_evrgrn$area == max(roi_evrgrn$area))

roi_conifer <- veg_poly %>%
  filter(Zone == z_conifer) %>%
  filter(veg_class == 8) %>%
  as_Spatial() %>%
  gBuffer(width = 1) %>%
  aggregate() %>%
  disaggregate()

roi_conifer$area <- area(roi_conifer)

roi_conifer <- subset(roi_conifer, roi_conifer$area == max(roi_conifer$area))

rm(rbr_ras, veg_poly, veg_ras, veg_zone, z)

writeOGR(
  obj = roi_evrgrn,
  dsn = 'data/grid_metrics',
  layer = 'ppwd_roi_evergreen',
  driver = 'ESRI Shapefile'
)

writeOGR(
  obj = roi_decid,
  dsn = 'data/grid_metrics',
  layer = 'ppwd_roi_deciduous',
  driver = 'ESRI Shapefile'
)

writeOGR(
  obj = roi_conifer,
  dsn = 'data/grid_metrics',
  layer = 'ppwd_roi_conifer',
  driver = 'ESRI Shapefile'
)

# ============= LAS height normalization and noise filter function ============== 

normnoise <- function(cluster, sensitivity, dtm) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  
  las <- normalize_height(las, dtm, na.rm = TRUE)
  
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  
  las <- filter_poi(las, buffer == 0)
  return(las)
}

ctg_to_las <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  return(las)
}

# ======================= Seperate height normalized uas data by vegetation type======================

z <- 8

dtm <- raster(glue(als_dtm_file))

uas <- glue(uas_las_file) %>%
  readLAScatalog()

opt_chunk_size(uas) <- 250
opt_chunk_buffer(uas) <- 30
opt_output_files(uas) <- 'data/las/uas_als-hnorm_z8_{ID}'
opt_select(uas) <- ''
opt_chunk_alignment(uas) <- c(uas$Min.X,uas$Min.Y)

plot(uas, chunk = TRUE)

uas <- catalog_apply(
  ctg = uas,
  FUN = normnoise,
  sensitivity = 1.2,
  dtm = dtm)

uas <- readLAScatalog(unlist(uas))

opt_chunk_buffer(uas) = 0
opt_select(uas) <- ''

plot(uas, chunk = TRUE)

uas <- catalog_sapply(
  ctg = uas,
  FUN = ctg_to_las)

writeLAS(uas, 'data/las/uas/ppwd_uas_z8_f2_hnorm-als.las')

decid_roi <- st_read('data/grid_metrics/ppwd_roi_deciduous.shp') %>%
  st_simplify() %>%
  st_buffer(dist = -2) %>%
  st_simplify()

decid_uas <- clip_roi(uas, decid_roi)

writeLAS(decid_uas, 'data/las/by_veg_type/ppwd_uas_decid.las')

evrgrn_roi <- st_read('data/grid_metrics/ppwd_roi_evergreen.shp') %>%
  st_simplify() %>%
  st_buffer(dist = -2) %>%
  st_simplify()

evrgrn_uas <- clip_roi(uas, evrgrn_roi)

writeLAS(evrgrn_uas, 'data/las/by_veg_type/ppwd_uas_evergreen.las')

conifer_roi <- st_read('data/grid_metrics/ppwd_roi_conifer.shp') %>%
  st_simplify() %>%
  st_buffer(dist = -2) %>%
  st_simplify()

conifer_uas <- clip_roi(uas, conifer_roi)

writeLAS(conifer_uas, 'data/las/by_veg_type/ppwd_uas_conifer.las')

decid_uas <- decid_uas$Z
evrgrn_uas <- evrgrn_uas$Z
conifer_uas <- conifer_uas$Z

rm(uas)


# ======================= Seperate height normalized als data by vegetation type======================

z <- 8

dtm <- raster(glue(als_dtm_file))

als <- glue(als_las_file) %>%
  readLAScatalog()

opt_chunk_size(als) <- 250
opt_chunk_buffer(als) <- 30
opt_output_files(als) <- 'data/las/als_als-hnorm_z8_{ID}'
opt_select(als) <- ''
opt_chunk_alignment(als) <- c(als$Min.X,als$Min.Y)

plot(als, chunk = TRUE)

als <- catalog_apply(
  ctg = als,
  FUN = normnoise,
  sensitivity = 1.2,
  dtm = dtm)

als <- readLAScatalog(unlist(als))

opt_chunk_buffer(als) = 0
opt_select(als) <- ''

plot(als, chunk = TRUE)

als <- catalog_sapply(
  ctg = als,
  FUN = ctg_to_las)

writeLAS(als, 'data/las/als/ppwd_als_z8_hnorm-als.las')

decid_als <- clip_roi(als, decid_roi)

writeLAS(decid_als, 'data/las/by_veg_type/ppwd_als_decid.las')

evrgrn_als <- clip_roi(als, evrgrn_roi)

writeLAS(evrgrn_als, 'data/las/by_veg_type/ppwd_als_evergreen.las')

conifer_als <- clip_roi(als, conifer_roi)

writeLAS(conifer_als, 'data/las/by_veg_type/ppwd_als_conifer.las')

decid_als <- decid_als$Z
evrgrn_als <- evrgrn_als$Z
conifer_als <- conifer_als$Z

rm(als)

# ================ Set ggplot theme ==========================

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

# ==================== Density plotting function =============================

dens_plot <- function(z_vector, x_label) {
  
  z_quant <- quantile(z_vector)
  
  ggplot()+ 
    geom_density(
      aes(
        x = z_vector)) +  
    annotate(
      geom = "text", 
      x = z_quant, 
      y = 0, 
      label = names(z_quant)) +
    geom_vline(
      xintercept = z_quant, 
      linetype = "longdash") +
    labs(
      y = 'Point Cloud Density',
      x = 'Vertical Height (m)'
    )
  
}

# ==================== Plot deciduous density plots =========================

write.csv(decid_uas, 'data/pntcld_density/ppwd_deciduous_uas.csv')
write.csv(decid_als, 'data/pntcld_density/ppwd_deciduous_als.csv')

write.csv(evrgrn_uas, 'data/pntcld_density/ppwd_evrgrn_uas.csv')
write.csv(evrgrn_als, 'data/pntcld_density/ppwd_evrgrn_als.csv')

write.csv(conifer_uas, 'data/pntcld_density/ppwd_conifer_uas.csv')
write.csv(conifer_als, 'data/pntcld_density/ppwd_conifer_als.csv')

dens_plot(decid_uas)

dens_plot(decid_als)

dens_plot(evrgrn_uas)

dens_plot(evrgrn_als)

dens_plot(conifer_uas)

dens_plot(conifer_als)
