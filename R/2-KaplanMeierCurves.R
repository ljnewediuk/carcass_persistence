
### Load packages ===
Packages <- c('data.table', 'coxme', 'survminer', 'survival', 'tidyverse')
lapply(Packages, require, character.only = TRUE)

### Load data ===
dat <- readRDS('input/Frogs_clean.rds')
# Final status
dat[, 'Status' := .(1)]

### Create discrete cols for temp and precip ===
# Calculate quantiles
Temp_low <- 22.15
Temp_high <- max(dat$Temperature)
Precip_low <- 3.7
Precip_high <- max(dat$Precipitation)
# Rank quantiles according to value for each row
# (upper/lower percentiles)
dat[, c('Temp_perc', 'Precip_perc') := .('med', 'med')]
for(i in 1:nrow(dat)){
  if(dat[i]$Precipitation >= Precip_low){
    dat[i]$Precip_perc <- 'high'
  }
  if(dat[i]$Precipitation <= Precip_low){
    dat[i]$Precip_perc <- 'low'
  }
  if(dat[i]$Temperature >= Temp_low){
    dat[i]$Temp_perc <- 'high'
  }
  if(dat[i]$Temperature <= Temp_low){
    dat[i]$Temp_perc <- 'low'
  }
}

### Plot ===
# Create survfit objects for plotting survival curves
Temperature_surv <- survfit(Surv(dat[Temp_perc=='high']$Days, 
                                 dat[Temp_perc=='high']$Status) ~ Temp_perc, 
                            data=dat[Temp_perc=='high'])
Precipitation_surv <- survfit(Surv(dat[Precip_perc=='high']$Days, 
                                   dat[Precip_perc=='high']$Status) ~ Precip_perc, 
                              data=dat[Precip_perc=='high'])
# Null survival without the influence of covariates
Null_surv <- survfit(Surv(dat$Days, dat$Status) ~ 1, data=dat)
# Reformat data for binding
dt <- data.table()
for(mod_type in c('Temperature', 'Precipitation', 'Null')){
  dt_mod <- na.omit(as.data.table(
    broom::tidy(get(paste(mod_type, 'surv', sep='_'))))) %>%
    mutate(type=mod_type, cum.haz=-log(estimate), 
           lower.haz=-log(conf.low), upper.haz=-log(conf.high)) %>%
    dplyr::select(time, type, estimate, conf.low, conf.high, 
                  cum.haz, lower.haz, upper.haz) %>%
    na.omit()
  dt <- rbind(dt, dt_mod)
}

# Approximate number of days for proportion carcasses removed: 50%, 75%, 90%
# Functions for temperature, precipitation, null approximations
t_approx <- approxfun(dt[dt$type == 'Temperature' ,]$estimate, 
                      dt[dt$type == 'Temperature' ,]$time)
p_approx <- approxfun(dt[dt$type == 'Precipitation' ,]$estimate, 
                      dt[dt$type == 'Precipitation' ,]$time)
n_approx <- approxfun(dt[dt$type == 'Null' ,]$estimate, 
                      dt[dt$type == 'Null' ,]$time)
# Data frames for plotting segments (note: number of days for temperature 
# and null curve slightly less different from 90%/75% to account for overlap)
t_surv <- data.frame (
  perc = c('p50', 'p75', 'p90'),
  y1 = 0,
  y2 = c(0.5, 0.25, 0.1),
  x = c(t_approx(0.5), t_approx(0.25), t_approx(0.11))
)
p_surv <- data.frame (
  perc = c('p50', 'p75', 'p90'),
  y1 = 0,
  y2 = c(0.5, 0.25, 0.1),
  x = c(p_approx(0.5), p_approx(0.25), p_approx(0.1))
)
n_surv <- data.frame (
  perc = c('p50', 'p75', 'p90'),
  y1 = 0,
  y2 = c(0.5, 0.25, 0.1),
  x = c(n_approx(0.5), n_approx(0.24), n_approx(0.1))
)

#  Plot
# tiff('figures/kaplan_meier.tiff', width = 6, height = 6, units = 'in', res = 300)
ggplot() +
  geom_segment(data = n_surv, aes(x = x, xend = x, y = y1, yend = y2), 
               colour = '#000000', linetype = 'dashed', lineend = 'round', 
               size = c(2, 1.25, 0.75)) +
  geom_segment(data = p_surv, aes(x = x, xend = x, y = y1, yend = y2), 
               colour = '#2150fb', linetype = 'solid', lineend = 'round', 
               size = c(2, 1.25, 0.75), alpha = c(0.75, 0.5, 0.25)) +
  geom_segment(data = t_surv, aes(x = x, xend = x, y = y1, yend = y2), 
               colour = '#ff4a4a', linetype = 'solid', lineend = 'round', 
               size = c(2, 1.25, 0.75), alpha = c(0.75, 0.5, 0.25)) +
  geom_ribbon(data=dt, aes(x=time, ymin=conf.low, ymax=conf.high, 
                           group=type, linetype=type, fill=type, 
                           alpha=type, size=type), colour=NA) +
  geom_line(data=dt, aes(x=time, y=estimate, group=type, 
                         linetype=type, colour=type, size=factor(type)), alpha=1) +
  scale_colour_manual(values=c('#000000', '#2150fb', '#ff4a4a')) +
  scale_fill_manual(values=c('#c0c0c0', '#2150fb', '#ff4a4a')) +
  scale_linetype_manual(values=c('dashed', 'solid', 'solid')) +
  scale_alpha_manual(values=c(0.3, 0.2, 0.2)) + 
  scale_size_manual(values=c(0.5,1,1)) +
  theme(panel.background=element_rect(fill='white', colour='black'), 
        axis.text=element_text(size=15, colour='black'),
        axis.title.y=element_text(size=15, colour='black', vjust=4), 
        axis.title.x=element_text(size=15, colour='black', vjust=-2),
        panel.grid=element_blank(), plot.margin=unit(c(0.5,0.5,1,1),  'cm'),
        legend.position=c(.75,.75), 
        legend.title=element_blank(), legend.key.size=unit(1, 'cm'),
        legend.key=element_rect(fill=NA, colour=NA), 
        legend.text=element_text(colour='black', size=15)) +
  xlab('Number of days') +  ylab('Probability of carcass persistence')


dev.off()

