# ===============================================================================
#
# CSF parameter testing data visualization
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 4 Sept 2020
# Last commit: 4 Sept 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Graphical visualization of CSF performance across parameter ranges. Input
# error summaries from csf_parameter-testing_data-compile.R. Graphs each parameter
# separately and combines into one multipart figure. Axis scales for each round 
# can be adjusted in user inputs within respective levles of if statement.
#
# ===============================================================================
# 
# User inputs:

# rnd = CSF parameter testing round
# csf_error_file = .csv file name skeleton (including {rnd} round glue placeholder)
#   for CSF parameter results by zone (output from csf_parameter-testing_data-compile.R)
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
library(ggplot2)
library(ggpubr)

# ================================= User inputs =================================

rnd <- 1
csf_error_file <- 'data/dtm/csf_parameter-testing/ppwd_csf-parameter-testing_rnd{rnd}_site-error.csv'

# ================================ ggplot theme ================================= 

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

# ========================= ggplot generation function ========================== 

csf_visualization <- function(parameter, x_title, df = csf_error, grid_position = c('left_axis', 'internal'), n_ticks = 3) {
  
  y_limits <- c(0, ceiling(max(df$site_rmse, na.rm = TRUE)))
  
  df <- df %>%
    filter(testing_parameter == parameter)
  
  x_limits <- c(0,
                df %>%
                  select(parameter) %>%
                  max(na.rm = TRUE))
  
  y_breaks = c(
    seq(
      from = y_limits[1], 
      to = y_limits[2], 
      length.out = n_ticks), 
    min(df$site_rmse))
  
  if (grid_position == 'left_axis') {
    y_labels = y_breaks %>%
      sprintf('%.2f', .)
  } else if (grid_position == 'internal') {
    y_labels = c(
      rep('', length(y_breaks)-1),
      sprintf('%.2f', tail(y_breaks, 1)))
  }
 
  
  ggplot(
    data = df,
    mapping = aes_(
      x = as.name(parameter))
    ) +
    geom_hline(
      yintercept = min(df$site_rmse),
      color = 'grey',
      size = 1,
      linetype = 'dashed'
    ) +
    geom_smooth(
      mapping = aes(
        y = grass_rmse),
      color = '#DDCC77',
      size = 1,
      se = FALSE
    ) +
    geom_smooth(
      mapping = aes(
        y = shrub_rmse),
      color = '#CC6677',
      size = 1,
      se = FALSE
    ) +
    geom_smooth(
      mapping = aes(
        y = decid_rmse),
      color = '#88CCEE',
      size = 1,
      se = FALSE
    ) +
    geom_smooth(
      mapping = aes(
        y = evrgrn_rmse),
      color = '#332288',
      size = 1,
      se = FALSE
    ) +
    geom_smooth(
      mapping = aes(
        y = conifer_rmse),
      color = '#117733',
      size = 1,
      se = FALSE
    ) +
    geom_point(
      mapping = aes(
        y = site_rmse,),
      size = 2
    ) +
    geom_line(
      mapping = aes(
        y = site_rmse),
      size = 1
    ) +
    geom_point(
      data = filter(df, site_rmse == min(site_rmse)),
      mapping = aes(
        y = site_rmse),
      size = 2,
      color = 'red'
    ) +
    labs(
      y = NULL,
      x = x_title
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels) +
    coord_cartesian(ylim = y_limits, xlim = x_limits)

}

# ================================== Plot data ================================== 

csf_error <- glue(csf_error_file) %>%
  read_csv()

cloth_resolution <- csf_visualization(
  parameter = 'cloth_resolution',
  x_title = 'Grid resolution',
  grid_position = 'left_axis')

time_step <- csf_visualization(
  parameter = 'time_step',
  x_title = 'Time step',
  grid_position = 'internal')

class_threshold <- csf_visualization(
  parameter = 'class_threshold',
  x_title = 'Distance threshold',
  grid_position = 'left_axis')

ndvi <- csf_visualization(
  parameter = 'ndvi_filter',
  x_title = 'NDVI threshold',
  grid_position = 'internal') 

rigidness <- csf_visualization(
  parameter = 'cloth_rigidness',
  x_title = 'Rigidness',
  grid_position = 'internal')



# =============================== Round one: Compile figure =============================== 

if (rnd == 1) {
  
  rigidness <- rigidness +
    coord_cartesian(
      ylim = rigidness$coordinates$limits$y,
      xlim = c(1,3)) +
    scale_x_continuous(breaks = c(1:3))
  
  cloth_resolution <- cloth_resolution +
    coord_cartesian(
      ylim = cloth_resolution$coordinates$limits$y,
      xlim = c(0, 2.5))
  
  ndvi <- ndvi +
    scale_x_continuous(labels = c('None', seq(0.25, 1, 0.25)))
  
  fig <- ggarrange(
    cloth_resolution, time_step, rigidness, class_threshold, ndvi,
    nrow = 2,
    ncol = 3,
    widths = c(1, 1, 0.75))
  
  fig <- annotate_figure(fig, left = text_grob('DTM RMSE (m)', family = 'serif', size = 16, rot = 90))
  
  fig
  
  ggsave(filename = glue('figures/csf_parameter-testing_rnd{rnd}.png'), width = 8.5, height = 5, units = 'in', dpi = 400)
  
}

# ===============================================================================