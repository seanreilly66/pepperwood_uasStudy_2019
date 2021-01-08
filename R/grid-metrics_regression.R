# ==============================================================================
#
# Grid metric regression linear modelling
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 16 Aug 2020
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
# Generates tables of regression linear modelling results from grid metrics. First
# section computes relationship between lidar and uas measurements of metrics from
# Filippelli et al. (2019). Second section computes impact of Tubbs fire on those
# metrics determined to have strong correlation in part one. In this case, it looks
# at the 95th height percentile before and after the Tubbs fire. Third section 
# compares ALS ladder fuels to burn severity in the Tubbs fire. Fourth section 
# looks at ladder fuel metric comparisons between UAS and ALS in unburned forest.
#
# ==============================================================================
# 
# User inputs:
#
# grid_metrics_file = .csv file containing compiled grid metrics, vegetation classes,
#   and RBR classes.
# 
# ==============================================================================
# 
# Package dependences: 
#
# tidyverse, ggplot2, glue
# 
# ==============================================================================
#
# Known problems:
#
# Documentation incomplete. 
# Output location hardcoded. 
# Script incomplete.
#
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(glue)
library(ggpubr)
library(FSA)

# ================================= User inputs ================================

grid_metrics_file <- 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_compiled-data.csv'

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
        legend.margin = margin(0,5,0,5),
        title = element_text(size = 12.8)
  )
)

# ================================ Prep dataset ================================

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(uas_20m.grid_ladder_fuel, uas_20m.grid_d00, uas_20m.grid_d01, uas_20m.grid_d02, 
         uas_20m.grid_zmax, uas_20m.grid_zmean, uas_20m.grid_zq5, uas_20m.grid_zq25, 
         uas_20m.grid_zq50, uas_20m.grid_zq75, uas_20m.grid_zq95, uas_20m.grid_zsd, 
         uas_20m.grid_zskew, uas_20m.grid_zkurt, als_20m.grid_ladder_fuel, 
         als_20m.grid_d00, als_20m.grid_d01, als_20m.grid_d02, als_20m.grid_zmax, 
         als_20m.grid_zmean, als_20m.grid_zq5, als_20m.grid_zq25, als_20m.grid_zq50,
         als_20m.grid_zq75, als_20m.grid_zq95, als_20m.grid_zsd, als_20m.grid_zskew, 
         als_20m.grid_zkurt, veg_class, rbr_class)

set.seed(305)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(rbr_class <= 2) %>%
  filter(veg_class > 5.75) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(veg_class = fct_recode(
    veg_class,
    'Deciduous broadleaf' = '6',
    'Evergreen broadleaf' = '7',
    'Conifer' = '8')) %>%
  group_by(veg_class) %>%
  sample_n(size = 30)

decid <- grid_data %>%
  filter(veg_class == 'Deciduous broadleaf')

evrgrn <- grid_data %>%
  filter(veg_class == 'Evergreen broadleaf')

conifer <- grid_data %>%
  filter(veg_class == 'Conifer')

allforest <- grid_data %>%
  mutate(veg_class = 'All forests')

metric_names <- names(grid_data) %>%
  str_extract('(?<=_).+') %>%
  unique() %>%
  str_subset('[^(class)]')

rm(grid_data)

# ======================== Regression modelling function =======================

lm_model_fun <- function(df, metric) {
  
  r <- cor(
    x = df[glue('uas_', metric)],
    y = df[glue('als_', metric)],
    method = 'pearson') %>%
    as.numeric()
  
  if (is.na(r)) {
    return(
      result = data.frame(
        metric = metric,
        coef = NA,
        se = NA,
        p = NA,
        r = NA))
  }
  
  lm_model <- lm(
    formula = glue('als_{metric} ~ uas_{metric}'), 
    data = df)
  
  lm_model_coef = summary(lm_model)$coefficients
  
  result = data.frame(
    metric = metric,
    coef = lm_model_coef[2, 'Estimate'],
    se = lm_model_coef[2, 'Std. Error'],
    p = lm_model_coef[2, 'Pr(>|t|)'],
    r = r )
  
  return(result)
  
}

# ====================== Generate regression result table ======================

lm_low_rbr <- data.frame(
  metric = as.character(),
  conifer_coef = as.numeric(),
  conifer_se = as.numeric(),
  conifer_p = as.numeric(),
  conifer_r = as.numeric(),
  decid_coef = as.numeric(),
  decid_se = as.numeric(),
  decid_p = as.numeric(),
  decid_r = as.numeric(),
  evrgrn_coef = as.numeric(),
  evrgrn_se = as.numeric(),
  evrgrn_p = as.numeric(),
  evrgrn_r = as.numeric(),
  allforest_coef = as.numeric(),
  allforest_se = as.numeric(),
  allforest_p = as.numeric(),
  allforest_r = as.numeric()
)

for (metric in metric_names) {
  
  conifer_result <- lm_model_fun(df = conifer, metric = metric) %>%
    rename(conifer_coef = coef,
           conifer_se = se,
           conifer_p = p,
           conifer_r = r)
  
  decid_result <- lm_model_fun(df = decid, metric = metric) %>%
    rename(decid_coef = coef,
           decid_se = se,
           decid_p = p,
           decid_r = r)
  
  evrgrn_result <- lm_model_fun(df = evrgrn, metric = metric) %>%
    rename(evrgrn_coef = coef,
           evrgrn_se = se,
           evrgrn_p = p,
           evrgrn_r = r)
  
  allforest_result <- lm_model_fun(df = allforest, metric = metric) %>%
    rename(allforest_coef = coef,
           allforest_se = se,
           allforest_p = p,
           allforest_r = r)
  
  lm_low_rbr <- lm_low_rbr %>%
    add_row(
      conifer_result %>%
        full_join(decid_result) %>%
        full_join(evrgrn_result) %>%
        full_join(allforest_result))

}

# ===================== Format statistics for publication ======================

sig_asterisk <- function(p_val) {
  ast = rep('', length(p_val))
  
  for (i in 1:length(p_val)) {
    if (p_val[i] > 0.1) {
      ast[i] = ''
    } else if (p_val[i] <= 0.1 & p_val[i] > 0.05) {
      ast[i] = '*'
    } else if (p_val[i] <= 0.05 & p_val[i] > 0.01) {
      ast[i] = '**'
    } else if (p_val[i] <= 0.01) {
      ast[i] = '***'
    }
  }
  
  return(ast)
  
} 

r_sign <- function(r_val) {
  r_val <- as.character(r_val)
  
  r_val[str_detect(r_val, '^[:digit:]')] <- glue("+{r_val[str_detect(r_val, '^[:digit:]')]}")
  
  return(r_val)
}

r_val = lm_low_rbr$decid_r
r_sign(r_val)

lm_low_rbr <- lm_low_rbr %>%
  mutate(metric = str_extract(metric, '(?<=d_).+')) %>%
  mutate_at(c(2, 3, 5:7, 9:11, 13:15, 17), round, digits = 2) %>%
  mutate(conifer_coef = glue('{conifer_coef}{sig_asterisk(conifer_p)}')) %>%
  mutate(decid_coef = glue('{decid_coef}{sig_asterisk(decid_p)}')) %>%
  mutate(evrgrn_coef = glue('{evrgrn_coef}{sig_asterisk(evrgrn_p)}')) %>%
  mutate(allforest_coef = glue('{allforest_coef}{sig_asterisk(allforest_p)}')) %>%
  mutate(conifer_r = glue('{r_sign(conifer_r)}')) %>%
  mutate(decid_r = glue('{r_sign(decid_r)}')) %>%
  mutate(evrgrn_r = glue('{r_sign(evrgrn_r)}')) %>%
  mutate(allforest_r = glue('{r_sign(allforest_r)}'))

ladder_r = lm_low_rbr %>%
  filter(metric == 'ladder_fuel') %>%
  select(conifer_r, evrgrn_r, decid_r, allforest_r)

lm_low_rbr <- lm_low_rbr %>%
  select(metric, 
         conifer_coef, conifer_r, 
         decid_coef, decid_r, 
         evrgrn_coef, evrgrn_r,
         allforest_coef, allforest_r)

write_excel_csv(lm_low_rbr, 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_regression_low-rbr.csv')

rm(conifer_result, decid_result, evrgrn_result, metric, lm_low_rbr)

# =========================== Plot ladder fuels data ===========================

plot_data <- rbind(allforest, conifer, decid, evrgrn) %>%
  mutate_at('veg_class', factor, levels = c('All forests',
                                            'Conifer',
                                            'Evergreen broadleaf',
                                            'Deciduous broadleaf'))


ladder_plot <- ggplot(
  data = plot_data,
  mapping = aes(x = uas_20m.grid_ladder_fuel,
                y = als_20m.grid_ladder_fuel,
                color = veg_class)
) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.3) +
  labs(x = 'UAS-SfM ladder fuel',
       y = 'ALS ladder fuel') +
  ylim(0, 1) +
  xlim(0, 1) +
  scale_color_manual(
    name = NULL,
    values = c('#CC6677', '#117733', '#332288', '#88CCEE'),
    labels = c(
      glue('All forests (r = {ladder_r$allforest_r})'),
      glue('Conifer forest (r = {ladder_r$conifer_r})'),
      glue('Evergreen broadleaf forest (r = {ladder_r$evrgrn_r})'),
      glue('Deciduous broadleaf forest (r = {ladder_r$decid_r})')
    )
  ) +
  theme(legend.position = c(0.48, 0.9))

ladder_plot

ggsave(
  filename = 'figures/fig8_als_vs_uas_ladderfuels.png',
  width = 4.5, 
  height = 4.5, 
  units = 'in', 
  dpi = 700)

# ==============================================================================