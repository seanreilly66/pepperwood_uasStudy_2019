library(lidR)
library(tidyverse)
library(glue)
library(doParallel)


zone <- c(2:4, 6:13)

icp_mat <- 'data/icp_free_analysis/icp_matrices/dtmref.xlsx'

als_dtm_file <- 'data/dtm/als/ppwd_als_z{z}_dtm.tif'
uas_las_file <- 
  'data/icp_free_analysis/ground_points/ppwd_uas_z{z}_f2_noicp_grndpts.las'

hnorm_las_output <- 
  'data/las/uas/ppwd_uas_z{z}_f2_noicp_hnorm-dtm_dtmref.las'

hnorm_grnd_df_output <- 
  'data/icp_free_analysis/ground_points/ppwd_noicp_grndpts_hnorm_dtmref.csv'


cl <- makeCluster(4)
registerDoParallel(cl)

hnorm_grnd_pts <- foreach (
  z = zone,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  source('R/las_transformation.R')
  
  t_mat <- readxl::read_xlsx(
    path = glue(icp_mat),
    sheet = glue('z{z}'),
    col_names = FALSE
  ) %>%
    as.matrix()
  
  dtm <- glue(als_dtm_file) %>%
    raster()
  
  
  las <- lastransformation(las_file = glue(uas_las_file),
                           t_matrix = t_mat)
  
  # WRITE LAS HERE
  
  # --------- Extract ground points and dtm Z for offset confirmation --------
  
  grnd <- las %>%
    filter_ground()
  
  grnd <- grnd %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z')
  
  # --------------------- DTM based height normalization ---------------------
  
  las <- normalize_height(las, dtm, na.rm = TRUE)
  
  writeLAS(las, glue(hnorm_las_output))
  
  # ---------------------- Output ground points dataset ----------------------
  
  grnd <- grnd@data %>%
    rename(uas_z = Z) %>%
    select(uas_z, dtm_z) %>%
    add_column(zone = z)
  
}

write_csv(hnorm_grnd_pts, glue(hnorm_grnd_df_output))

stopCluster(cl)
