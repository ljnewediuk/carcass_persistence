
### Load packages ===
Packages <- c('data.table', 'coxme', 'survminer', 'survival')
lapply(Packages, require, character.only = TRUE)

### Load data ===
dat <- readRDS('input/Frogs_clean.rds')

# Set NLFR as reference species
dat$f_Species = relevel(factor(dat$Species), ref = "NLFR")

### Add surv anal columns ===
# Final status
dat[, 'Status' := .(1)]
# Censor column
status <- Surv(dat$Days, dat$Status)

### Define covariates ===
# Condition covariates: Carcass condition, first julian day
condition_covs <- c('CF', 'JDayF')
# Carcass covariates: Snout-vent length, species
carcass_covs <- c('SVL*f_Species')
# Weather covariates:  Temperature, precipitaiton, temperature*precipitation
weather_covs <- c('Precipitation*Temperature')
# Traffic covariates: Mean daily traffic volume
traffic_covs <- c('LF',  'MDTV')

### Fit models ===
# Fit models for each hypothesis
global_mod <- coxph(reformulate(c(condition_covs, carcass_covs, weather_covs, traffic_covs), response='status'), data=dat, na.action='na.fail')
null_mod <- coxph(status~1, data=dat)
condition_mod <- coxph(reformulate(c(condition_covs), response='status'), data=dat)
carcass_mod <- coxph(reformulate(c(carcass_covs), response='status'), data=dat)
weather_mod <- coxph(reformulate(c(weather_covs), response='status'), data=dat)
traffic_mod <- coxph(reformulate(c(traffic_covs), response='status'), data=dat)
# Tabulate AICc
AICc_tab <- data.table()
for(vars in c('global', 'null', 'condition', 'carcass', 'weather', 'traffic')){
  AICc_mod <- data.table(model=vars, AICc=AIC(get(paste(vars, 'mod', sep='_'))))
  AICc_tab <- rbind(AICc_tab, AICc_mod)
}
# Order
AICc_tab <- AICc_tab[order(AICc)]

### Predict survival for temperature and precipitation ===
# Generate new data with other variable set to median
pred_temp <- as.data.table(expand.grid(Temperature=seq(min(dat$Temperature), max(dat$Temperature), length.out = 91), Precipitation=median(dat$Precipitation)))
pred_precip<- as.data.table(expand.grid(Precipitation=seq(min(dat$Precipitation), max(dat$Precipitation), length.out = 91), Temperature=median(dat$Temperature)))

# Predict hazards
pred_temp[, c('Hazard', 'SE')  := .(
  predict(weather_mod, newdata=pred_temp, type="risk"), 
  predict(weather_mod, newdata=pred_temp, type="risk", se.fit=TRUE)$se.fit)]
pred_precip[, c('Hazard', 'SE')  := .(
  predict(weather_mod, newdata=pred_precip, type="risk"), 
  predict(weather_mod, newdata=pred_precip, type="risk", se.fit=TRUE)$se.fit)]

# Predict survival probability
pred_temp[, 'Probability'  := .(
  exp(-predict(weather_mod, newdata=pred_temp, type="expected"))
  )]
pred_precip[, 'Probability'  := .(
  exp(-predict(weather_mod, newdata=pred_precip, type="expected"))
)]

# Plot hazards
# tiff('figures/temp_hazard.tiff', width = 6, height = 6, units = 'in', res = 300)
ggplot()+
  geom_hline(yintercept=1, linetype='dashed') +
  geom_line(data=pred_temp, aes(x=Temperature,y=Hazard), size=1) + 
  geom_ribbon(data=pred_temp, aes(x=Temperature, ymin = Hazard-SE, ymax = Hazard+SE), colour = NA,alpha = 0.3, fill="grey4") +
  theme(panel.background=element_rect(fill='white', colour='black'), axis.text=element_text(size=15, colour='black'),
        axis.title.y=element_text(size=15, colour='black', vjust=4), axis.title.x=element_text(size=15, colour='black', vjust=-2),
        panel.grid=element_blank(), plot.margin=unit(c(0.5,0.5,1,1),  'cm')) +
  ylab('Hazard ratio') + xlab('Temperature (Celsius)')

# tiff('figures/precip_hazard.tiff', width = 6, height = 6, units = 'in', res = 300)
ggplot()+
  geom_hline(yintercept=1, linetype='dashed') +
  geom_line(data=pred_precip, aes(x=Precipitation,y=Hazard), size=1) + 
  geom_ribbon(data=pred_precip, aes(x=Precipitation, ymin = Hazard-SE, ymax = Hazard+SE), colour = NA,alpha = 0.3, fill="grey4") +
  theme(panel.background=element_rect(fill='white', colour='black'), axis.text=element_text(size=15, colour='black'),
        axis.title.y=element_text(size=15, colour='black', vjust=4), axis.title.x=element_text(size=15, colour='black', vjust=-2),
        panel.grid=element_blank(), plot.margin=unit(c(0.5,0.5,1,1),  'cm')) +
  ylab('Hazard ratio') + xlab('Precipitation/day (mm)')

# Plot survival probabilities
pred_temp %>%
  mutate(Count = 100*Probability) %>%
  ggplot(aes(x = Temperature, y = Count)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = 'lm', colour = 'black') +
  theme_bw() +
  labs(y = 'Expected number of individuals found/100', x = 'Temperature °C')

# Save model csvs
write.csv(AICc_tab, 'tables/AICc_table.csv')
write.csv(broom::tidy(carcass_mod, conf.int = T), 'tables/carcass_coeffs.csv')
write.csv(broom::tidy(condition_mod, conf.int = T), 'tables/condition_coeffs.csv')
write.csv(broom::tidy(traffic_mod, conf.int = T), 'tables/traffic_coeffs.csv')
write.csv(broom::tidy(weather_mod, conf.int = T), 'tables/weather_coeffs.csv')
write.csv(broom::tidy(global_mod, conf.int = T), 'tables/global_coeffs.csv')

