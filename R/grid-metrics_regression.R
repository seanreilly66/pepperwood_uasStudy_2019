# ===============================================================================
#
# Grid metric regression linear modelling
#
# ===============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 16 Aug 2020
# Last commit: 17 Aug 2020
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ===============================================================================
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
# ===============================================================================
# 
# User inputs:
#
# grid_metrics_file = .csv file containing compiled grid metrics, vegetation classes,
#   and RBR classes.
# 
# ===============================================================================
# 
# Package dependences: 
#
# tidyverse, ggplot2, glue
# 
# ===============================================================================
#
# Known problems:
#
# Documentation incomplete. 
# Output location hardcoded. 
# Script incomplete.
#
# ===============================================================================

library(tidyverse)
library(ggplot2)
library(glue)
library(ggpubr)

# ================================= User inputs =================================

grid_metrics_file <- 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_compiled-data.csv'

# ======================== Regression modelling function ======================== 

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

# ===============================================================================
# ===================== Part One: Low RBR metric comparison ===================== 
# ===============================================================================

# ================================ Prep dataset ================================= 

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(c(4:9, 15:17, 27:53, 59:61, 71:94))

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(rbr_class <= 2) %>%
  mutate_at(c('veg_class', 'rbr_class', 'topo_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class', 'topo_class'), as_factor) %>%
  group_by(veg_class) %>%
  sample_n(size = 30) 

decid <- grid_data %>%
  filter(veg_class == 6)

evrgrn <- grid_data %>%
  filter(veg_class == 7)

conifer <- grid_data %>%
  filter(veg_class == 8)

metric_names <- names(grid_data) %>%
  str_extract('(?<=_).+') %>%
  unique() %>%
  str_subset('[^(class)]')

rm(grid_data)

# ====================== Generate regression result table ======================= 

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
  evrgrn_r = as.numeric())

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
  
  lm_low_rbr <- lm_low_rbr %>%
    add_row(
      conifer_result %>%
        full_join(decid_result) %>%
        full_join(evrgrn_result))
  
}

write.csv(lm_low_rbr, 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_regression_low-rbr.csv')

rm(conifer, conifer_result, decid, decid_result, evrgrn, evrgrn_result, metric)

# ===============================================================================
# ======================= Part Two: Impact of Tubbs Fire ======================== 
# ===============================================================================

# =========== Isolate metrics with significant coefficients near one ============ 

metrics <- lm_low_rbr %>%
  filter_at(c('conifer_p', 'evrgrn_p', 'decid_p'), all_vars(.<0.1)) %>%
  filter_at(c('conifer_coef', 'evrgrn_coef', 'decid_coef'), all_vars(.> 0.8)) %>%
  .$metric %>%
  as.character()

rm(lm_low_rbr)

# ================================ Prep dataset ================================= 

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(paste0('uas_', metrics), paste0('als_', metrics), veg_class, rbr_class)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(veg_class >= 6) %>%
  filter(rbr_class%%1 < 0.25 | rbr_class%%1 > 0.75) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(veg_class = fct_recode(
    veg_class,
    'Deciduous Broadleaf' = '6',
    'Evergreen Broadleaf' = '7',
    'Conifer' = '8')) %>%
  mutate(rbr_class = fct_recode(
    rbr_class,
    'Unchanged' = '1',
    'Low' = '2',
    'Moderate' = '3',
    'High' = '4'))

grid_data <- grid_data %>%
  transmute(zmax_dif = als_zmax - uas_zmax,
         zq95_dif = uas_zq95 - als_zq95,
         veg_class = veg_class,
         rbr_class = rbr_class)

decid <- grid_data %>%
  filter(veg_class == 'Deciduous Broadleaf') %>%
  group_by(rbr_class) %>%
  sample_n(30)

evrgrn <- grid_data %>%
  filter(veg_class == 'Evergreen Broadleaf') %>%
  filter(rbr_class != 'High') %>%
  group_by(rbr_class) %>%
  sample_n(30)

rm(grid_data, metrics)

# ============================== Plot differences =============================== 

decid_plot <- ggplot(data = decid) +
  geom_boxplot(
    aes(
      x = rbr_class,
      y = zq95_dif)) +
  labs(
    x = NULL,
    y = 'Difference in p95 (ALS - UAS)') + 
  ylim(-30, 10)

evrgrn_plot <- ggplot(data = evrgrn) +
  geom_boxplot(
    aes(
      x = rbr_class,
      y = zq95_dif)) +
  labs(
    x = NULL,
    y = NULL) + 
  ylim(-30, 10) + 
  theme(axis.text.y = element_blank())

fig <- ggarrange(
  decid_plot, evrgrn_plot,
  nrow = 1, ncol = 2, widths = c(1,0.75),
  labels = list('(a)','(b)'), 
  font.label = list(family = 'serif', size = 16, face = 'plain'), 
  label.x = c(0.17, 0.06))

fig <- annotate_figure(
  fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_dif-p95.png',
  width = 8, 
  height = 4, 
  units = 'in', 
  dpi = 400)

rm(decid_plot, evrgrn_plot, fig)

# ============================ Annova of differences ============================ 

group_by(decid, rbr_class) %>%
  summarize(
    n = n(),
    mean = mean(zq95_dif, na.rm = TRUE),
    sd = sd(zq95_dif, na.rm = TRUE))

decid_aov <- aov(zq95_dif ~ rbr_class, data = decid)
summary(decid_aov)

TukeyHSD(decid_aov)


group_by(evrgrn, rbr_class) %>%
  summarize(
    n = n(),
    mean = mean(zq95_dif, na.rm = TRUE),
    sd = sd(zq95_dif, na.rm = TRUE))

evrgrn_aov <- aov(zq95_dif ~ rbr_class, data = evrgrn)
summary(evrgrn_aov)

TukeyHSD(evrgrn_aov)

rm(decid, decid_aov, evrgrn, evrgrn_aov)

# ===============================================================================
# =================== Part Three: Ladder fuels in Tubbs Fire ==================== 
# ===============================================================================

# ================================ Prep dataset ================================= 

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(uas_ladder_fuel, als_ladder_fuel, veg_class, rbr_class)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(veg_class >= 6) %>%
  filter(rbr_class%%1 < 0.25 | rbr_class%%1 > 0.75) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(veg_class = fct_recode(
    veg_class,
    'Deciduous Broadleaf' = '6',
    'Evergreen Broadleaf' = '7',
    'Conifer' = '8')) %>%
  mutate(rbr_class = fct_recode(
    rbr_class,
    'Unchanged' = '1',
    'Low' = '2',
    'Moderate' = '3',
    'High' = '4'))

decid <- grid_data %>%
  filter(veg_class == 'Deciduous Broadleaf') %>%
  group_by(rbr_class) %>%
  sample_n(30)

evrgrn <- grid_data %>%
  filter(veg_class == 'Evergreen Broadleaf') %>%
  filter(rbr_class != 'High') %>%
  group_by(rbr_class) %>%
  sample_n(30, replace = TRUE)

rm(grid_data)

# =================== Plot ALS ladder fuel with fire severity =================== 

decid_plot <- ggplot(data = decid) +
  geom_boxplot(
    aes(
      x = rbr_class,
      y = als_ladder_fuel)) +
  labs(
    x = NULL,
    y = 'Ladder fuel percentage') + 
  ylim(0,1)

decid_plot

evrgrn_plot <- ggplot(data = evrgrn) +
  geom_boxplot(
    aes(
      x = rbr_class,
      y = als_ladder_fuel)) +
  labs(
    x = NULL,
    y = NULL) + 
  ylim(0,1) + 
  theme(axis.text.y = element_blank())

evrgrn_plot

fig <- ggarrange(
  decid_plot, evrgrn_plot,
  nrow = 1, ncol = 2, widths = c(1,0.75),
  labels = list('(a)','(b)'), 
  font.label = list(family = 'serif', size = 16, face = 'plain'), 
  label.x = c(0.17, 0.06))

fig <- annotate_figure(
  fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_als-ladder-fuel.png',
  width = 8, 
  height = 4, 
  units = 'in', 
  dpi = 400)

rm(decid_plot, evrgrn_plot, fig)

# ================= ANOVA of ALS ladder fuel with fire severity ================= 

group_by(decid, rbr_class) %>%
  summarize(
    n = n(),
    mean = mean(als_ladder_fuel, na.rm = TRUE),
    sd = sd(als_ladder_fuel, na.rm = TRUE))

decid_aov <- aov(als_ladder_fuel ~ rbr_class, data = decid)
summary(decid_aov)

TukeyHSD(decid_aov)


group_by(evrgrn, rbr_class) %>%
  summarize(
    n = n(),
    mean = mean(als_ladder_fuel, na.rm = TRUE),
    sd = sd(als_ladder_fuel, na.rm = TRUE))

evrgrn_aov <- aov(als_ladder_fuel ~ rbr_class, data = evrgrn)
summary(evrgrn_aov)

TukeyHSD(evrgrn_aov)

rm(decid, decid_aov, evrgrn, evrgrn_aov)
