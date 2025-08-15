
library(tidyverse)
library(cowplot)

# Load plots
surv_prob_plot <- readRDS('output/surv_prob_plot.rds')
est_mort_plot <- readRDS('output/est_mort_plot.rds')

plot_grid(surv_prob_plot, est_mort_plot, labels = 'AUTO', label_size = 15)

# Save figure
ggsave('figures/sim_plot.tiff', plot = last_plot(), 
       device = 'tiff', dpi = 300, height = 16, width = 34, units = 'cm')
