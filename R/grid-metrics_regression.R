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
library(FSA)

# ================================= User inputs =================================

grid_metrics_file <- 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_compiled-data.csv'

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
# ===============================================================================
# ===================== Part One: Low RBR metric comparison ===================== 
# ===============================================================================
# ===============================================================================

# ================================ Prep dataset ================================= 

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(c(4:9, 15:17, 27:53, 59:61, 71:94))

set.seed(4850)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(rbr_class <= 2) %>%
  mutate_at(c('veg_class', 'rbr_class', 'topo_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class', 'topo_class'), as_factor) %>%
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

metric_names <- names(grid_data) %>%
  str_extract('(?<=_).+') %>%
  unique() %>%
  str_subset('[^(class)]')

rm(grid_data)

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

lm_low_rbr <- lm_low_rbr %>%
  mutate(metric = str_extract(metric, '(?<=d_).+')) %>%
  filter(metric %in% c('ladder_fuel', 'd00', 'd01', 'd02', 'zmax', 'zmean', 'zq5', 'zq25', 'zq50',
                       'zq75', 'zq95', 'zsd', 'zskew', 'zkurt')) %>%
  mutate_at(c(2,3,5:7,9:11,13), round, digits = 2) %>%
  mutate(conifer_coef = glue('{conifer_coef}({conifer_se}){sig_asterisk(conifer_p)}')) %>%
  mutate(decid_coef = glue('{decid_coef}({decid_se}){sig_asterisk(decid_p)}')) %>%
  mutate(evrgrn_coef = glue('{evrgrn_coef}({evrgrn_se}){sig_asterisk(evrgrn_p)}'))

ladder_r = lm_low_rbr %>%
  filter(metric == 'ladder_fuel') %>%
  select(conifer_r, evrgrn_r, decid_r)

lm_low_rbr <- lm_low_rbr %>%
  select(metric, conifer_coef, conifer_r, decid_coef, decid_r, evrgrn_coef, evrgrn_r)

write.csv(lm_low_rbr, 'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_regression_low-rbr.csv')

rm(conifer_result, decid_result, evrgrn_result, metric, lm_low_rbr)

# =========================== Plot ladder fuels data ============================ 

ladder_plot <- ggplot(
  data = rbind(decid, evrgrn, conifer) %>%
    mutate(veg_class = fct_relevel(
      veg_class,
      'Conifer', 'Evergreen broadleaf', 'Deciduous broadleaf')),
  mapping = aes(
    x = uas_20m.grid_ladder_fuel,
    y = als_20m.grid_ladder_fuel,
    color = veg_class)) +
  geom_point(size = 2) +
  geom_smooth(
    method = 'lm', 
    se = FALSE,
    size = 1) +
  labs(
    x = 'UAS ladder fuel',
    y = 'ALS ladder fuel') + 
  ylim(0,1) +
  xlim(0,1) +
  scale_color_manual(
    name = NULL,
    values = c('#117733', '#332288', '#88CCEE'),
    labels = c(glue('Conifer (R = {ladder_r$conifer_r})'),
               glue('Evergreen broadleaf (R = {ladder_r$evrgrn_r})'),
               glue('Deciduous broadleaf (R = {ladder_r$decid_r})'))) +
  theme(legend.position = c(0.48,0.9))

ladder_plot

ggsave(
  filename = 'figures/uas-ladder-fuel_vs_als-ladder-fuel.png',
  width = 4.5, 
  height = 4.5, 
  units = 'in', 
  dpi = 400)


# ===============================================================================
# ===============================================================================
# ======================= Part Two: Impact of Tubbs Fire ======================== 
# ===============================================================================
# ===============================================================================

# ================================ Prep dataset ================================= 

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(uas_20m.grid_zq95, als_20m.grid_zq95, 
         uas_20m.grid_zq75, als_20m.grid_zq75, 
         als_20m.grid_ladder_fuel, veg_class, rbr_class)

set.seed(305)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class%%1 < 0.25 | veg_class%%1 > 0.75) %>%
  filter(veg_class >= 5.75) %>%
  filter(rbr_class%%1 < 0.25 | rbr_class%%1 > 0.75) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(veg_class = fct_recode(
    veg_class,
    'Deciduous broadleaf' = '6',
    'Evergreen broadleaf' = '7',
    'Conifer' = '8')) %>%
  mutate(rbr_class = fct_recode(
    rbr_class,
    'None' = '1',
    'Low' = '2',
    'Med' = '3',
    'High' = '4')) %>%
  filter( (veg_class != 'Deciduous broadleaf')|(rbr_class != 'High') ) %>%
  group_by(veg_class, rbr_class) %>%
  sample_n(30)

grid_data <- grid_data %>%
  transmute(
    zq95_dif = uas_20m.grid_zq95 - als_20m.grid_zq95,
    zq75_dif = uas_20m.grid_zq75 - als_20m.grid_zq75,
    ladder_fuels = als_20m.grid_ladder_fuel,
    veg_class = veg_class,
    rbr_class = rbr_class)

conifer <- grid_data %>%
  filter(veg_class == 'Conifer')

decid <- grid_data %>%
  filter(veg_class == 'Deciduous broadleaf')

evrgrn <- grid_data %>%
  filter(veg_class == 'Evergreen broadleaf')

rm(grid_data)

# ===============================================================================
# ==================== Fire impact on 95th percentile height ==================== 
# ===============================================================================

# ============================= Plotting function =============================== 

rbr_p95_plot <- function(data_set, group_labels) {
  
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(zq95_dif < 10) %>%
    summarize(
      y = max(zq95_dif) + 1
    ) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_hline(
      yintercept = 0,
      color = 'grey80',
      size = 1,
      linetype = 'dashed') +
    geom_boxplot(
      aes(
        x = rbr_class,
        y = zq95_dif,
        fill = rbr_class)) +
    labs(
      x = 'RBR severity',
      y = bquote(''~Delta ~P[95]~' (post-pre fire)')) +
    ylim(-30, 10) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) + 
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class, 
          y = y, 
          label = label),
      vjust=0,
      family = 'serif', 
      fontface = 'plain',
      size = 4.5) 
  
  return(fig)
  
}

# ============================= Conifer comparison ==============================

conifer_p95_kruskal <- kruskal.test(zq95_dif ~ rbr_class, data = conifer)
conifer_p95_kruskal

conifer_p95_dunn <- dunnTest(zq95_dif ~ rbr_class, data = conifer, method = "bonferroni")
conifer_p95_dunn

conifer_p95_labels <- c('e', 'e', 'f', 'f')

conifer_p95_plot <- rbr_p95_plot(conifer, conifer_p95_labels)
conifer_p95_plot

# ============================ Evergreen comparison ============================= 

evrgrn_p95_kruskal <- kruskal.test(zq95_dif ~ rbr_class, data = evrgrn)
evrgrn_p95_kruskal

evrgrn_p95_dunn <- dunnTest(zq95_dif ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_p95_dunn

evrgrn_p95_labels <- c('e,f', 'e', 'f', 'g')

evrgrn_p95_plot <- rbr_p95_plot(evrgrn, evrgrn_p95_labels)
evrgrn_p95_plot

# ============================ Deciduous comparison ============================= 

decid_p95_kruskal <- kruskal.test(zq95_dif ~ rbr_class, data = decid)
decid_p95_kruskal

decid_p95_dunn <- dunnTest(zq95_dif ~ rbr_class, data = decid, method = "bonferroni")
decid_p95_dunn

decid_p95_labels <- c('e', 'e', 'e')

decid_p95_plot <- rbr_p95_plot(decid, decid_p95_labels)
decid_p95_plot

# ================================ Combine plots ================================ 

conifer_p95_plot <- conifer_p95_plot +
  labs(x = NULL) +
  coord_cartesian(ylim = c(-20, 10))

evrgrn_p95_plot <- evrgrn_p95_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(-20, 10))

decid_p95_plot <- decid_p95_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(-20, 10))

p95_fig <- ggarrange(
  conifer_p95_plot, evrgrn_p95_plot, decid_p95_plot, 
  nrow = 1, ncol = 3, widths = c(1, 0.8, 0.62),
  labels = list('Conifer','Evergreen broadleaf', 'Deciduous broadleaf'), 
  font.label = list(family = 'serif', size = 13, face = 'plain'), 
  label.x = c(0.17, -0.23, -0.31))

p95_fig <- annotate_figure(
  p95_fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

p95_fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_dif-p95.png',
  width = 7, 
  height = 3.5, 
  units = 'in', 
  dpi = 400)

rm(decid_p95_dunn, decid_p95_kruskal, evrgrn_p95_dunn, evrgrn_p95_kruskal, 
   conifer_p95_dunn, conifer_p95_kruskal, rbr_p95_plot)

# ===============================================================================
# ==================== Fire impact on 75th percentile height ==================== 
# ===============================================================================

# ============================= Plotting function =============================== 

rbr_p75_plot <- function(data_set, group_labels) {
  
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(zq75_dif < 10) %>%
    summarize(
      y = max(zq75_dif) + 1
    ) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_hline(
      yintercept = 0,
      color = 'grey80',
      size = 1,
      linetype = 'dashed') +
    geom_boxplot(
      aes(
        x = rbr_class,
        y = zq75_dif,
        fill = rbr_class)) +
    labs(
      x = 'RBR severity',
      y = bquote(''~Delta ~P[75]~' (post-pre fire)')) +
    ylim(-30, 10) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) + 
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class, 
          y = y, 
          label = label),
      vjust=0,
      family = 'serif', 
      fontface = 'plain',
      size = 4.5) 
  
  return(fig)
  
}

# ============================= Conifer comparison ==============================

conifer_p75_kruskal <- kruskal.test(zq75_dif ~ rbr_class, data = conifer)
conifer_p75_kruskal

conifer_p75_dunn <- dunnTest(zq75_dif ~ rbr_class, data = conifer, method = "bonferroni")
conifer_p75_dunn

conifer_p75_labels <- c('e', 'e', 'f', 'f')

conifer_p75_plot <- rbr_p75_plot(conifer, conifer_p75_labels)
conifer_p75_plot

# ============================ Evergreen comparison ============================= 

evrgrn_p75_kruskal <- kruskal.test(zq75_dif ~ rbr_class, data = evrgrn)
evrgrn_p75_kruskal

evrgrn_p75_dunn <- dunnTest(zq75_dif ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_p75_dunn

evrgrn_p75_labels <- c('e', 'e', 'f', 'g')

evrgrn_p75_plot <- rbr_p75_plot(evrgrn, evrgrn_p75_labels)
evrgrn_p75_plot

# ============================ Deciduous comparison ============================= 

decid_p75_kruskal <- kruskal.test(zq75_dif ~ rbr_class, data = decid)
decid_p75_kruskal

decid_p75_dunn <- dunnTest(zq75_dif ~ rbr_class, data = decid, method = "bonferroni")
decid_p75_dunn

decid_p75_labels <- c('e', 'e', 'e')

decid_p75_plot <- rbr_p75_plot(decid, decid_p75_labels)
decid_p75_plot

# ================================ Combine plots ================================ 

conifer_p75_plot <- conifer_p75_plot +
  labs(x = NULL)

evrgrn_p75_plot <- evrgrn_p75_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

decid_p75_plot <- decid_p75_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

p75_fig <- ggarrange(
  conifer_p75_plot, evrgrn_p75_plot, decid_p75_plot, 
  nrow = 1, ncol = 3, widths = c(1, 0.8, 0.62),
  labels = list('Conifer','Evergreen broadleaf', 'Deciduous broadleaf'), 
  font.label = list(family = 'serif', size = 13, face = 'plain'), 
  label.x = c(0.17, -0.23, -0.31))

p75_fig <- annotate_figure(
  p75_fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

p75_fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_dif-p75.png',
  width = 7, 
  height = 3.5, 
  units = 'in', 
  dpi = 400)

rm(decid_p75_dunn, decid_p75_kruskal, evrgrn_p75_dunn, evrgrn_p75_kruskal, 
   conifer_p75_dunn, conifer_p75_kruskal, rbr_p75_plot)

# ============================ Compile stacked plot =============================

conifer_p95_plot <- conifer_p95_plot +
  theme(axis.text.x = element_blank())

evrgrn_p95_plot <- evrgrn_p95_plot +
  theme(axis.text.x = element_blank())

decid_p95_plot <- decid_p95_plot +
  theme(axis.text.x = element_blank())

p_fig <- ggarrange(
  conifer_p95_plot, evrgrn_p95_plot, decid_p95_plot,
  conifer_p75_plot, evrgrn_p75_plot, decid_p75_plot, 
  nrow = 2, ncol = 3, widths = c(1, 0.8, 0.62),
  labels = list('Conifer','Evergreen broadleaf', 'Deciduous broadleaf'), 
  font.label = list(family = 'serif', size = 13, face = 'plain'), 
  label.x = c(0.17, -0.23, -0.31))

p_fig <- annotate_figure(
  p_fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

p_fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_p-dif.png',
  width = 7, 
  height = 5, 
  units = 'in', 
  dpi = 400)




# ===============================================================================
# ==================== Ladder fuels impact on fire severity ===================== 
# ===============================================================================

# ============================= Plotting function =============================== 

rbr_ladder_plot <- function(data_set, group_labels) {
  
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(ladder_fuels < 1) %>%
    summarize(
      y = max(ladder_fuels) + 0.02
    ) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_boxplot(
      aes(
        x = rbr_class,
        y = ladder_fuels,
        fill = rbr_class)) +
    labs(
      x = 'RBR severity',
      y = bquote('Pre-fire ladder fuels')) +
    ylim(0, 1) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) + 
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class, 
          y = y, 
          label = label),
      vjust=0,
      family = 'serif', 
      fontface = 'plain',
      size = 5)
  
  return(fig)
}

# ============================= Conifer comparison ==============================

conifer_kruskal <- kruskal.test(ladder_fuels ~ rbr_class, data = conifer)
conifer_kruskal

conifer_dunn <- dunnTest(ladder_fuels ~ rbr_class, data = conifer, method = "bonferroni")
conifer_dunn

conifer_labels <- c('e,f', 'e', 'f', 'e,f')

conifer_plot <- rbr_ladder_plot(conifer, conifer_labels)
conifer_plot

# ============================ Evergreen comparison ============================= 

evrgrn_kruskal <- kruskal.test(ladder_fuels ~ rbr_class, data = evrgrn)
evrgrn_kruskal

evrgrn_dunn <- dunnTest(ladder_fuels ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_dunn

evrgrn_labels <- c('e', 'e', 'e', 'f')

evrgrn_plot <- rbr_ladder_plot(evrgrn, evrgrn_labels)
evrgrn_plot

# ============================ Deciduous comparison ============================= 

decid_kruskal <- kruskal.test(ladder_fuels ~ rbr_class, data = decid)
decid_kruskal

decid_dunn <- dunnTest(ladder_fuels ~ rbr_class, data = decid, method = "bonferroni")
decid_dunn

decid_labels <- c('e', 'e', 'e')

decid_plot <- rbr_ladder_plot(decid, decid_labels)
decid_plot

# ================================ Combine plots ================================ 

conifer_plot <- conifer_plot +
  labs(x = NULL)

evrgrn_plot <- evrgrn_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

decid_plot <- decid_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

fig <- ggarrange(
  conifer_plot, evrgrn_plot, decid_plot, 
  nrow = 1, ncol = 3, widths = c(1, 0.8, 0.62),
  labels = list('(a)','(b)', '(c)'), 
  font.label = list(family = 'serif', size = 16, face = 'plain'), 
  label.x = c(0.25, .03, 0.03))

fig <- annotate_figure(
  fig, 
  bottom = text_grob('RBR Severity', family = 'serif', size = 16))

fig

ggsave(
  filename = 'figures/tubbs-fire_rbr_vs_als-ladder-fuel.png',
  width = 6.5, 
  height = 3.5, 
  units = 'in', 
  dpi = 400)

# ===============================================================================