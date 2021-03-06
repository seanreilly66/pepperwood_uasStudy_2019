# ==============================================================================
#
# Plots of the Tubbs fire impact through uas and als structure metrics
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 16 Aug 2020
# Last commit: 7 Jan 2021
#
# Status: Under development
#
# This file created as part of 2019 Pepperwood UAS study
#
# ==============================================================================
#
# Description:
#
# Computes impact of Tubbs fire on those metrics determined to have strong
# correlation between uas and als datasets. Also looks at NDVI with burn severity.
# Compares ALS ladder fuels to burn severity in the Tubbs fire.
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

grid_metrics_file <-
  'data/grid_metrics/ppwd_hnorm-als_grid-metrics_20m-grid_compiled-data.csv'
logfile <-
  'data/grid_metrics/ppwd_tubbs_impact_statistics_output.txt'

# ============================== Set ggplot theme ==============================

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
    legend.margin = margin(0, 5, 0, 5),
    title = element_text(size = 12.8)
  )
)

# ================================ Prep dataset ================================

grid_data <- data.table::fread(grid_metrics_file)

grid_data <- grid_data %>%
  select(
    uas_20m.grid_zq95,
    als_20m.grid_zq95,
    uas_20m.grid_zq75,
    als_20m.grid_zq75,
    als_20m.grid_ladder_fuel,
    uas_20m.grid_p75_ndvi,
    veg_class,
    rbr_class
  )

set.seed(305)

grid_data <- grid_data %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter(veg_class %% 1 < 0.25 | veg_class %% 1 > 0.75) %>%
  filter(veg_class >= 5.75) %>%
  filter(rbr_class %% 1 < 0.25 | rbr_class %% 1 > 0.75) %>%
  mutate_at(c('veg_class', 'rbr_class'), round) %>%
  mutate_at(c('veg_class', 'rbr_class'), as_factor) %>%
  mutate(
    veg_class = fct_recode(
      veg_class,
      'Deciduous broadleaf' = '6',
      'Evergreen broadleaf' = '7',
      'Conifer' = '8'
    )
  ) %>%
  mutate(rbr_class = fct_recode(
    rbr_class,
    'None' = '1',
    'Low' = '2',
    'Med' = '3',
    'High' = '4'
  )) %>%
  filter((veg_class != 'Deciduous broadleaf') |
           (rbr_class != 'High')) %>%
  group_by(veg_class, rbr_class) %>%
  sample_n(30)

grid_data <- grid_data %>%
  transmute(
    zq95_dif = uas_20m.grid_zq95 - als_20m.grid_zq95,
    zq75_dif = uas_20m.grid_zq75 - als_20m.grid_zq75,
    ladder_fuels = als_20m.grid_ladder_fuel,
    p75_ndvi = uas_20m.grid_p75_ndvi,
    veg_class = veg_class,
    rbr_class = rbr_class
  )

conifer <- grid_data %>%
  filter(veg_class == 'Conifer')

decid <- grid_data %>%
  filter(veg_class == 'Deciduous broadleaf')

evrgrn <- grid_data %>%
  filter(veg_class == 'Evergreen broadleaf')

allforest <- grid_data %>%
  mutate(veg_class = 'All forests')

rm(grid_data)

# ============================== Initiate log file =============================

write(
  glue(
    '\n=====================================================================\n',
    'Statistic results for Tubbs fire impact',
    '\n=====================================================================\n\n',
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    '\nauthor: Sean Reilly',
    '\n\n',
    'Corresponds with figures 9 and 10 from uas_capability_manuscript',
    '\nProduced using tubbs_fire_impact_plots.R [https://bit.ly/3njOrm0]',
    '\n\n\n'
  ),
  file = logfile,
  append = FALSE
)


# ==============================================================================
# ==================== Fire impact on 95th percentile height ===================
# ==============================================================================

write(
  glue(
    '\n=====================================================================\n',
    'Part 1: Fire impact on 95th percentile height',
    '\n=====================================================================\n'
  ),
  file = logfile,
  append = TRUE
)

# ============================= Plotting function ==============================

rbr_p95_plot <- function(data_set, group_labels) {
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(zq95_dif < 10) %>%
    summarize(y = max(zq95_dif) + 1) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_hline(
      yintercept = 0,
      color = 'grey80',
      size = 1,
      linetype = 'dashed'
    ) +
    geom_boxplot(aes(x = rbr_class,
                     y = zq95_dif,
                     fill = rbr_class)) +
    labs(x = 'RBR fire severity',
         y = bquote('' ~ Delta ~ P[95] ~ ' height (m)')) +
    ylim(-30, 10) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) +
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class,
          y = y,
          label = label),
      vjust = 0,
      family = 'serif',
      fontface = 'plain',
      size = 4.5
    )
  
  return(fig)
  
}

# ============================= Conifer comparison =============================
write(
  glue(
    '\n\nConifer forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

conifer_p95_kruskal <-
  kruskal.test(zq95_dif ~ rbr_class, data = conifer)
conifer_p95_kruskal

conifer_p95_dunn <-
  dunnTest(zq95_dif ~ rbr_class, data = conifer, method = "bonferroni")
conifer_p95_dunn

conifer_p95_labels <- c('e', 'e', 'e,f', 'f')

conifer_p95_plot <- rbr_p95_plot(conifer, conifer_p95_labels)
conifer_p95_plot

sink()

# ============================ Evergreen comparison ============================

write(
  glue(
    '\n\nEvergreen broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

evrgrn_p95_kruskal <-
  kruskal.test(zq95_dif ~ rbr_class, data = evrgrn)
evrgrn_p95_kruskal

evrgrn_p95_dunn <-
  dunnTest(zq95_dif ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_p95_dunn

evrgrn_p95_labels <- c('e,f', 'e', 'f', 'g')

evrgrn_p95_plot <- rbr_p95_plot(evrgrn, evrgrn_p95_labels)
evrgrn_p95_plot

sink()

# ============================ Deciduous comparison ============================

write(
  glue(
    '\n\nDeciduous broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

decid_p95_kruskal <-
  kruskal.test(zq95_dif ~ rbr_class, data = decid)
decid_p95_kruskal

decid_p95_dunn <-
  dunnTest(zq95_dif ~ rbr_class, data = decid, method = "bonferroni")
decid_p95_dunn

decid_p95_labels <- c('e', 'e', 'e')

decid_p95_plot <- rbr_p95_plot(decid, decid_p95_labels)
decid_p95_plot

sink()

# =========================== All forests comparison ===========================

write(
  glue(
    '\n\nAll forests',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

allforest_p95_kruskal <-
  kruskal.test(zq95_dif ~ rbr_class, data = allforest)
allforest_p95_kruskal

allforest_p95_dunn <-
  dunnTest(zq95_dif ~ rbr_class, data = allforest, method = "bonferroni")
allforest_p95_dunn

allforest_p95_labels <- c('e,f', 'e', 'f', 'g')

allforest_p95_plot <- rbr_p95_plot(allforest, allforest_p95_labels)
allforest_p95_plot

sink()

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

allforest_p95_plot <-allforest_p95_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(-20, 10))


p95_fig <- ggarrange(
  conifer_p95_plot,
  evrgrn_p95_plot,
  decid_p95_plot,
  allforest_p95_plot,
  nrow = 1,
  ncol = 4,
  widths = c(1, 0.8, 0.62, 0.8),
  labels = list(
    'Conifer forest',
    'Evergreen\nbroadleaf forest',
    'Deciduous\nbroadleaf forest',
    'All forests'
  ),
  font.label = list(
    family = 'serif',
    size = 13,
    face = 'plain'
  ),
  label.x = c(0.3, 0.1, 0.15, 0.1),
  label.y = 0.98,
  hjust = 'bottom',
  vjust = 'right'
)

p95_fig <- annotate_figure(p95_fig,
                           bottom = text_grob('RBR fire severity', family = 'serif', size = 16))

p95_fig

# ggsave(
#   filename = 'figures/tubbs-fire-rbr_vs_dif-p95.png',
#   width = 9,
#   height = 3.5,
#   units = 'in',
#   dpi = 700
# )

rm(
  decid_p95_dunn,
  decid_p95_kruskal,
  evrgrn_p95_dunn,
  evrgrn_p95_kruskal,
  conifer_p95_dunn,
  conifer_p95_kruskal,
  rbr_p95_plot
)

# ==============================================================================
# ==================== Fire impact on 75th percentile height ===================
# ==============================================================================

write(
  glue(
    '\n\n',
    '\n=====================================================================\n',
    'Part 2: Fire impact on 75th percentile height',
    '\n=====================================================================\n'
  ),
  file = logfile,
  append = TRUE
)

# ============================= Plotting function ==============================

rbr_p75_plot <- function(data_set, group_labels) {
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(zq75_dif < 10) %>%
    summarize(y = max(zq75_dif) + 1) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_hline(
      yintercept = 0,
      color = 'grey80',
      size = 1,
      linetype = 'dashed'
    ) +
    geom_boxplot(aes(x = rbr_class,
                     y = zq75_dif,
                     fill = rbr_class)) +
    labs(x = 'RBR fire severity',
         y = bquote('' ~ Delta ~ P[75] ~ ' height (m)')) +
    ylim(-30, 10) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) +
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class,
          y = y,
          label = label),
      vjust = 0,
      family = 'serif',
      fontface = 'plain',
      size = 4.5
    )
  
  return(fig)
  
}

# ============================= Conifer comparison =============================

write(
  glue(
    '\n\nConifer forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

conifer_p75_kruskal <-
  kruskal.test(zq75_dif ~ rbr_class, data = conifer)
conifer_p75_kruskal

conifer_p75_dunn <-
  dunnTest(zq75_dif ~ rbr_class, data = conifer, method = "bonferroni")
conifer_p75_dunn

conifer_p75_labels <- c('e', 'e', 'f', 'f')

conifer_p75_plot <- rbr_p75_plot(conifer, conifer_p75_labels)
conifer_p75_plot

sink()

# ============================ Evergreen comparison ============================

write(
  glue(
    '\n\nEvergreen broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

evrgrn_p75_kruskal <-
  kruskal.test(zq75_dif ~ rbr_class, data = evrgrn)
evrgrn_p75_kruskal

evrgrn_p75_dunn <-
  dunnTest(zq75_dif ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_p75_dunn

evrgrn_p75_labels <- c('e', 'e', 'f', 'g')

evrgrn_p75_plot <- rbr_p75_plot(evrgrn, evrgrn_p75_labels)
evrgrn_p75_plot

sink()

# ============================ Deciduous comparison ============================

write(
  glue(
    '\n\nDeciduous broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

decid_p75_kruskal <-
  kruskal.test(zq75_dif ~ rbr_class, data = decid)
decid_p75_kruskal

decid_p75_dunn <-
  dunnTest(zq75_dif ~ rbr_class, data = decid, method = "bonferroni")
decid_p75_dunn

decid_p75_labels <- c('e', 'e', 'e')

decid_p75_plot <- rbr_p75_plot(decid, decid_p75_labels)
decid_p75_plot

sink()

# =========================== All forests comparison ===========================

write(
  glue(
    '\n\nAll forests broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

allforest_p75_kruskal <-
  kruskal.test(zq75_dif ~ rbr_class, data = allforest)
allforest_p75_kruskal

allforest_p75_dunn <-
  dunnTest(zq75_dif ~ rbr_class, data = allforest, method = "bonferroni")
allforest_p75_dunn

allforest_p75_labels <- c('e', 'e', 'f', 'g')

allforest_p75_plot <- rbr_p75_plot(allforest, allforest_p75_labels)
allforest_p75_plot

sink()

# ================================ Combine plots ================================

conifer_p75_plot <- conifer_p75_plot +
  labs(x = NULL)

evrgrn_p75_plot <- evrgrn_p75_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

decid_p75_plot <- decid_p75_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

allforest_p75_plot <- allforest_p75_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

p75_fig <- ggarrange(
  conifer_p75_plot,
  evrgrn_p75_plot,
  decid_p75_plot,
  allforest_p75_plot,
  nrow = 1,
  ncol = 4,
  widths = c(1, 0.8, 0.62, 0.8),
  labels = list(
    'Conifer forest',
    'Evergreen\nbroadleaf forest',
    'Deciduous\nbroadleaf forest',
    'All forests'
  ),
  font.label = list(
    family = 'serif',
    size = 13,
    face = 'plain'
  ),
  label.x = c(0.3, 0.1, 0.15, 0.1),
  label.y = 0.98,
  hjust = 'bottom',
  vjust = 'right'
)

p75_fig <- annotate_figure(p75_fig,
                           bottom = text_grob('RBR Severity', family = 'serif', size = 16))

p75_fig

# ggsave(
#   filename = 'figures/tubbs-fire-rbr_vs_dif-p75.png',
#   width = 9,
#   height = 3.5,
#   units = 'in',
#   dpi = 700
# )

rm(
  decid_p75_dunn,
  decid_p75_kruskal,
  evrgrn_p75_dunn,
  evrgrn_p75_kruskal,
  conifer_p75_dunn,
  conifer_p75_kruskal,
  rbr_p75_plot
)

# ===============================================================================
# ===================== Fire impact on 75th percentile NDVI =====================
# ===============================================================================

write(
  glue(
    '\n\n',
    '\n=====================================================================\n',
    'Part 3: Fire impact on 75th percentile NDVI',
    '\n=====================================================================\n'
  ),
  file = logfile,
  append = TRUE
)

# ============================= Plotting function ===============================

rbr_ndvi_plot <- function(data_set, group_labels) {
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(p75_ndvi < 1) %>%
    summarize(y = max(p75_ndvi) + 0.02) %>%
    add_column(label = group_labels)
  
  fig <- ggplot(data = data_set) +
    geom_boxplot(aes(x = rbr_class,
                     y = p75_ndvi,
                     fill = rbr_class)) +
    labs(x = 'RBR fire severity',
         y = bquote('Post-fire ' ~ P[75] ~ ' NDVI')) +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) +
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class,
          y = y,
          label = label),
      vjust = 0,
      family = 'serif',
      fontface = 'plain',
      size = 4.5
    )
  
  return(fig)
  
}

# ============================= Conifer comparison ==============================

write(
  glue(
    '\n\nConifer forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

conifer_ndvi_kruskal <-
  kruskal.test(p75_ndvi ~ rbr_class, data = conifer)
conifer_ndvi_kruskal

conifer_ndvi_dunn <-
  dunnTest(p75_ndvi ~ rbr_class, data = conifer, method = "bonferroni")
conifer_ndvi_dunn

conifer_ndvi_labels <- c('e', 'e', 'f', 'f')

conifer_ndvi_plot <- rbr_ndvi_plot(conifer, conifer_ndvi_labels)
conifer_ndvi_plot

sink()

# ============================ Evergreen comparison =============================

write(
  glue(
    '\n\nEvergreen broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

evrgrn_ndvi_kruskal <-
  kruskal.test(p75_ndvi ~ rbr_class, data = evrgrn)
evrgrn_ndvi_kruskal

evrgrn_ndvi_dunn <-
  dunnTest(p75_ndvi ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_ndvi_dunn

evrgrn_ndvi_labels <- c('e', 'e', 'f', 'f')

evrgrn_ndvi_plot <- rbr_ndvi_plot(evrgrn, evrgrn_ndvi_labels)
evrgrn_ndvi_plot

sink()

# ============================ Deciduous comparison =============================

write(
  glue(
    '\n\nDeciduous broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

decid_ndvi_kruskal <-
  kruskal.test(p75_ndvi ~ rbr_class, data = decid)
decid_ndvi_kruskal

decid_ndvi_dunn <-
  dunnTest(p75_ndvi ~ rbr_class, data = decid, method = "bonferroni")
decid_ndvi_dunn

decid_ndvi_labels <- c('e', 'e', 'f')

decid_ndvi_plot <- rbr_ndvi_plot(decid, decid_ndvi_labels)
decid_ndvi_plot

sink()

# =========================== All forests comparison ===========================

write(
  glue(
    '\n\nAll forests',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

allforest_ndvi_kruskal <-
  kruskal.test(p75_ndvi ~ rbr_class, data = allforest)
allforest_ndvi_kruskal

allforest_ndvi_dunn <-
  dunnTest(p75_ndvi ~ rbr_class, data = allforest, method = "bonferroni")
allforest_ndvi_dunn

allforest_ndvi_labels <- c('e', 'e', 'f', 'g')

allforest_ndvi_plot <- rbr_ndvi_plot(allforest, allforest_ndvi_labels)
allforest_ndvi_plot

sink()

# ================================ Combine plots ================================

conifer_ndvi_plot <- conifer_ndvi_plot +
  labs(x = NULL)

evrgrn_ndvi_plot <- evrgrn_ndvi_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

decid_ndvi_plot <- decid_ndvi_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

allforest_ndvi_plot <- allforest_ndvi_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())



ndvi_fig <- ggarrange(
  conifer_ndvi_plot,
  evrgrn_ndvi_plot,
  decid_ndvi_plot,
  allforest_ndvi_plot,
  nrow = 1,
  ncol = 4,
  widths = c(1, 0.8, 0.62, 0.8),
  labels = list(
    'Conifer forest',
    'Evergreen\nbroadleaf forest',
    'Deciduous\nbroadleaf forest',
    'All forests'
  ),
  font.label = list(
    family = 'serif',
    size = 13,
    face = 'plain'
  ),
  label.x = c(0.3, 0.1, 0.15, 0.1),
  label.y = 0.98,
  hjust = 'bottom',
  vjust = 'right'
)

ndvi_fig <- annotate_figure(ndvi_fig,
                            bottom = text_grob('RBR Severity', family = 'serif', size = 16))

ndvi_fig

# ggsave(
#   filename = 'figures/tubbs-fire-rbr_vs_p75-ndvi.png',
#   width = 7,
#   height = 3.5,
#   units = 'in',
#   dpi = 700
# )

rm(
  decid_ndvi_dunn,
  decid_ndvi_kruskal,
  evrgrn_ndvi_dunn,
  evrgrn_ndvi_kruskal,
  conifer_ndvi_dunn,
  conifer_ndvi_kruskal,
  rbr_ndvi_plot
)

# ============================ Compile stacked plot =============================

conifer_p95_plot <- conifer_p95_plot +
  theme(axis.text.x = element_blank()) +
  ggtitle('Conifer forest\nbroadleaf forest')

evrgrn_p95_plot <- evrgrn_p95_plot +
  theme(axis.text.x = element_blank()) +
  ggtitle('Evergreen\nbroadleaf forest')

decid_p95_plot <- decid_p95_plot +
  theme(axis.text.x = element_blank()) +
  ggtitle('Deciduous\nbroadleaf forest')

allforest_p95_plot <- allforest_p95_plot +
  theme(axis.text.x = element_blank()) +
  ggtitle('All forests\nbroadleaf forest')

conifer_p75_plot <- conifer_p75_plot +
  theme(axis.text.x = element_blank())

evrgrn_p75_plot <- evrgrn_p75_plot +
  theme(axis.text.x = element_blank())

decid_p75_plot <- decid_p75_plot +
  theme(axis.text.x = element_blank())

allforest_p75_plot <- allforest_p75_plot +
  theme(axis.text.x = element_blank())

p_fig <- ggarrange(
  conifer_p95_plot,
  evrgrn_p95_plot,
  decid_p95_plot,
  allforest_p95_plot,
  conifer_p75_plot,
  evrgrn_p75_plot,
  decid_p75_plot,
  allforest_p75_plot,
  conifer_ndvi_plot,
  evrgrn_ndvi_plot,
  decid_ndvi_plot,
  allforest_ndvi_plot,
  nrow = 3,
  ncol = 4,
  widths = c(1, 0.8, 0.62, 0.8),
  labels = list('A)', 'B)', 'C)', 'D)', 
                'E)', 'F)', 'G)', 'H)', 
                'I)', 'J)', 'K)', 'L)'),
  font.label = list(family = 'serif', face = 'plain'),
  label.x = c(0.25, 0.05, 0.05, 0.05,
              0.25, 0.05, 0.05, 0.05,
              0.25, 0.05, 0.06, 0.05),
  label.y = c(0.83, 0.83, 0.83, 0.83,
              1, 1, 1, 1,
              1, 1, 1, 1)
)

p_fig <- annotate_figure(p_fig,
                         bottom = text_grob('RBR fire severity', family = 'serif', size = 16))

p_fig

ggsave(
  filename = 'figures/fig10_tubbs_fire_impact_on_height_metrics.png',
  width = 9,
  height = 9,
  units = 'in',
  dpi = 700
)

# ==============================================================================
# ==================== Ladder fuels impact on fire severity ====================
# ==============================================================================

write(
  glue(
    '\n\n',
    '\n=====================================================================\n',
    'Part 4: Ladder fuels impact on fire severity',
    '\n=====================================================================\n'
  ),
  file = logfile,
  append = TRUE
)

# ============================= Plotting function ===============================

rbr_ladder_plot <- function(data_set, group_labels) {
  dunn_groups <- data_set %>%
    group_by(rbr_class) %>%
    filter(ladder_fuels < 0.85) %>%
    summarize(y = max(ladder_fuels) + 0.02) %>%
    add_column(label = group_labels)

  fig <- ggplot(data = data_set %>%
                  filter(ladder_fuels < 0.99)) +
    geom_boxplot(aes(x = rbr_class,
                     y = ladder_fuels,
                     fill = rbr_class)) +
    labs(x = 'RBR severity',
         y = bquote('Pre-fire ladder fuels')) +
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c('#828282', '#ffffbe', '#ffaa00', '#c80000')) +
    guides(fill = FALSE) +
    geom_text(
      data = dunn_groups,
      aes(x = rbr_class,
          y = y,
          label = label),
      vjust = 0,
      family = 'serif',
      fontface = 'plain',
      size = 5
    )

  return(fig)
}

# ============================= Conifer comparison =============================

write(
  glue(
    '\n\nConifer forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

conifer_ladder_kruskal <-
  kruskal.test(ladder_fuels ~ rbr_class, data = conifer)
conifer_ladder_kruskal

conifer_ladder_dunn <-
  dunnTest(ladder_fuels ~ rbr_class, data = conifer, method = "bonferroni")
conifer_ladder_dunn

conifer_ladder_labels <- c('e', 'f', 'e', 'e,f')

conifer_ladder_plot <-
  rbr_ladder_plot(conifer, conifer_ladder_labels)
conifer_ladder_plot

sink()

# ============================ Evergreen comparison ============================

write(
  glue(
    '\n\nEvergreen broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

evrgrn_ladder_kruskal <-
  kruskal.test(ladder_fuels ~ rbr_class, data = evrgrn)
evrgrn_ladder_kruskal

evrgrn_ladder_dunn <-
  dunnTest(ladder_fuels ~ rbr_class, data = evrgrn, method = "bonferroni")
evrgrn_ladder_dunn

evrgrn_ladder_labels <- c('e', 'e', 'e', 'f')

evrgrn_ladder_plot <- rbr_ladder_plot(evrgrn, evrgrn_ladder_labels)
evrgrn_ladder_plot

sink()

# ============================ Deciduous comparison ============================

write(
  glue(
    '\n\nDeciduous broadleaf forest',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

decid_ladder_kruskal <-
  kruskal.test(ladder_fuels ~ rbr_class, data = decid)
decid_ladder_kruskal

decid_ladder_dunn <-
  dunnTest(ladder_fuels ~ rbr_class, data = decid, method = "bonferroni")
decid_ladder_dunn

decid_ladder_labels <- c('e', 'e', 'e')

decid_ladder_plot <- rbr_ladder_plot(decid, decid_ladder_labels)
decid_ladder_plot

sink()

# =========================== All forest comparison ============================

write(
  glue(
    '\n\nAll forests',
    '\n---------------------------------------------------------------------\n'
  ),
  file = logfile,
  append = TRUE
)

sink(file = logfile, append = TRUE)

allforest_ladder_kruskal <-
  kruskal.test(ladder_fuels ~ rbr_class, data = allforest)
allforest_ladder_kruskal

allforest_ladder_dunn <-
  dunnTest(ladder_fuels ~ rbr_class, data = allforest, method = "bonferroni")
allforest_ladder_dunn

allforest_ladder_labels <- c('e,f', 'e', 'f', 'g')

allforest_ladder_plot <- rbr_ladder_plot(allforest, allforest_ladder_labels)
allforest_ladder_plot

sink()

# ================================ Combine plots ===============================

conifer_ladder_plot <- conifer_ladder_plot +
  labs(x = NULL)

evrgrn_ladder_plot <- evrgrn_ladder_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

decid_ladder_plot <- decid_ladder_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

allforest_ladder_plot <- allforest_ladder_plot +
  labs(y = NULL, x = NULL) +
  theme(axis.text.y = element_blank())

ladder_fig <- ggarrange(
  conifer_ladder_plot,
  evrgrn_ladder_plot,
  decid_ladder_plot,
  allforest_ladder_plot,
  nrow = 1,
  ncol = 4,
  widths = c(1, 0.8, 0.62, 0.8),
  labels = list(
    'Conifer forest',
    'Evergreen\nbroadleaf forest',
    'Deciduous\nbroadleaf forest',
    'All forests'
  ),
  font.label = list(
    family = 'serif',
    size = 13,
    face = 'plain'
  ),
  label.x = c(0.3, 0.1, 0.15, 0.1),
  label.y = 0.98,
  hjust = 'bottom',
  vjust = 'right'
)

ladder_fig <- annotate_figure(ladder_fig,
                              bottom = text_grob('RBR fire severity', family = 'serif', size = 16))

ladder_fig

ggsave(
  filename = 'figures/fig9_ladderfuel_impact_on_tubbs_fire.png',
  width = 9,
  height = 3.5,
  units = 'in',
  dpi = 700
)

# ===============================================================================