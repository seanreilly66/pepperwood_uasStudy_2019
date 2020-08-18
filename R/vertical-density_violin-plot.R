# ===============================================================================
#
# Vertical height density violin plot
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 17 Aug 2020
# Last commit: 18 Aug 2020
#
# Status: Complete
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
#
# Description:
#
# Generates violin plot of uas and als height kernal density by forest vegetation
# type from input height datasets
#
# ===============================================================================
# 
# User inputs:
#
# decid_uas = Deciduous UAS height data csv file
# decid_als = Deciduous ALS height data csv file
# evrgrn_uas = Evergreen UAS height data csv file
# evrgrn_als = Evergreen ALS height data csv file
# conifer_uas = conifer UAS height data csv file
# conifer_als = conifer ALS height data csv file
# 
# ===============================================================================
# 
# Package dependences: 
#
# tidyverse, ggplot2, glue, ggpubr
# 
# ===============================================================================
#
# Known problems:
#
# ===============================================================================

library(tidyverse)
library(ggplot2)
library(glue)
library(ggpubr)

# ================================= User inputs =================================

decid_uas <- 'data/pntcld_density/ppwd_deciduous_uas.csv'
decid_als <- 'data/pntcld_density/ppwd_deciduous_als.csv'

evrgrn_uas <- 'data/pntcld_density/ppwd_evrgrn_uas.csv'
evrgrn_als <- 'data/pntcld_density/ppwd_evrgrn_als.csv'

conifer_uas <- 'data/pntcld_density/ppwd_conifer_uas.csv'
conifer_als <- 'data/pntcld_density/ppwd_conifer_als.csv'

# ============================= Dataset combination ============================= 

height_df <- bind_rows(
  read.csv(decid_uas) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Deciduous broadleaf') %>%
    add_column(data_type = 'UAS'),
  read.csv(decid_als) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Deciduous broadleaf') %>%
    add_column(data_type = 'ALS'),
  read.csv(evrgrn_uas) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Evergreen broadleaf') %>%
    add_column(data_type = 'UAS'),
  read.csv(evrgrn_als) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Evergreen broadleaf') %>%
    add_column(data_type = 'ALS'),
  read.csv(conifer_uas) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Conifer') %>%
    add_column(data_type = 'UAS'),
  read.csv(conifer_als) %>%
    select(2) %>%
    rename(Z = 1) %>%
    add_column(veg_class = 'Conifer') %>%
    add_column(data_type = 'ALS')) %>%
  mutate_at(c('veg_class', 'data_type'), as.factor)

rm(decid_uas, decid_als, evrgrn_uas, evrgrn_als, conifer_uas, conifer_als)

# ============================== ggplot theme set =============================== 

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

# ============================ Generate violin plot ============================= 

fig <- ggplot(
  data = height_df,
  mapping = aes(
    x = veg_class,
    y = Z,
    fill = data_type)) + 
  geom_violin(
    draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(
    y = 'Height (m)',
    x = NULL) +
  ylim(0,30) +
  scale_fill_grey(
    start = 1,
    end = 0.8,
    name = NULL) +
  theme(legend.position = c(0.1, 0.9))

fig

ggsave(
  filename = 'figures/height_violin.png',
  width = 8, 
  height = 4, 
  units = 'in', 
  dpi = 400)

# ===============================================================================
