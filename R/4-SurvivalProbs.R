### Load packages ===
Packages <- c('data.table', 'coxme', 'survminer', 'survival', 'tidyverse')
lapply(Packages, require, character.only = TRUE)

### Load data ===
dat <- readRDS('input/Frogs_clean.rds')
# Final status
dat[, 'Status' := .(1)]

# Fit survival regression model
# predicted number of days a carcass should survive across temperatures and 
# precipitation levels

temp_surv_c <- survreg(Surv(Days, Status) ~ Temperature*Precipitation, 
                       data=dat)

# For sequences
min_temp <- quantile(dat$Temperature, 0.05)
max_temp <- quantile(dat$Temperature, 0.95)
min_precip <- quantile(dat$Precipitation, 0.05)
mean_precip <- mean(dat$Precipitation)
max_precip <- quantile(dat$Precipitation, 0.95)

# Function to predict survival from min to max temp across sequence of precip
tp_preds <- function(precip, length_seq) {
  pt <- predict(temp_surv_c, 
                newdata = list(
                  Temperature = seq(min_temp, max_temp, length.out = length_seq), 
                  # Holding precip constant at mean, but we could do mean, min,
                  # max precip and make a ribbon, or maybe gradient colour?
                  Precipitation = rep(precip, length_seq)), 
                type = 'response', se.fit = T)
    names(pt) <- c(paste0('p', round(precip, 0), 'mm_fit'), paste0('p', round(precip, 0), 'mm_se.fit'))
  return(pt)
}

# Sequence of precipitation
precip_seq <- list(low = min_precip, mid = mean_precip, high = max_precip)

# Data frame of number of survival days by temp for high, low, med precip
precip_temp_seq <- lapply(precip_seq, function(x) tp_preds(x, length_seq = 100)) %>%
  bind_cols() %>%
  mutate(Temperature = seq(min_temp, max_temp, length.out = 100))

# Plot
p <- precip_temp_seq %>%
  pivot_longer(-Temperature, 
               names_to = c("Precipitation", ".value"), 
               names_sep="_" ) %>%
  mutate(Precipitation = factor(Precipitation, levels = c('p16mm', 'p5mm', 'p1mm'),
                                labels = c('16 mm', '5 mm', '1 mm')),
         ci = se.fit * 1.96) %>%
  ggplot(aes(x = Temperature, y = fit, colour = Precipitation, group = Precipitation)) +
  scale_colour_manual(values = c('#003A6F', '#2150fb', '#80dfff')) +
  scale_fill_manual(values = c('#003A6F', '#2150fb', '#80dfff')) +
  geom_ribbon(aes(x = Temperature, ymin = fit - ci, ymax = fit + ci, fill = Precipitation), 
              alpha = 0.2, colour = NA) + 
  geom_line(linewidth = 1) + 
  theme(panel.background=element_rect(fill='white', colour='black'), 
        axis.text=element_text(size=15, colour='black'),
        axis.title.y=element_text(size=15, colour='black', vjust=4), 
        axis.title.x=element_text(size=15, colour='black', vjust=-2),
        panel.grid=element_blank(), plot.margin=unit(c(0.5,0.5,1,1),  'cm'),
        legend.position=c(.8,.75), 
        legend.title=element_text(size=12, colour='black'), 
        legend.key.size=unit(0.75, 'cm'),
        legend.key=element_rect(fill=NA, colour=NA), 
        legend.text=element_text(colour='black', size=12)) +
  labs(x = 'Temperature °C', y = 'Expected persistence on road (days)')

# Save the plot
saveRDS(p, 'output/surv_prob_plot.rds')
