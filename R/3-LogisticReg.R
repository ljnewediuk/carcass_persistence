
### Load packages ===
Packages <- c('tidyverse', 'emmeans')
lapply(Packages, require, character.only = TRUE)

### Load data ===
dat <- readRDS('input/Frogs_clean.rds')

# Create expanded data frame with a row for each day a carcass was present, and
# a final row when it was no longer present.
dat_expand <- data.frame()
for(i in 1:nrow(dat)) {
  row_x <- dat[i,] %>%
    slice(rep(1:ncol(dat), each = dat[i,]$Days + 1)) 
  row_x$Days <- seq(0, dat[i,]$Days, by = 1)
  row_x$Removed <- c(rep(0, nrow(row_x) -1), 1)
  row_x$Present <- c(rep(1, nrow(row_x) -1), 0)
  
  dat_expand <- bind_rows(dat_expand, row_x)
}

# Fit logistic regression model with temperature, precipitation, and days as 
# covariates
PT_glm <- glm(Present ~ Days + Precipitation*Temperature, 
              data = dat_expand, family = 'binomial')

# Make data frame for with Days 0-30, grouped by days
days_df <- data.frame(Days = seq(0, 30, by = 1)) %>%
  group_by(Days)

# Get quantiles for precip and temp
p_low <- quantile(dat_expand$Precipitation, 0.05)
p_high <- quantile(dat_expand$Precipitation, 0.95)
t_low <- quantile(dat_expand$Temperature, 0.05)
t_high <- quantile(dat_expand$Temperature, 0.95)

# For low precipitation and temp
preds_lowPT <- days_df %>%
  mutate(Precipitation = p_low, Temperature = t_low) %>%
  group_split() %>%
  # Predict from days 0-30
  lapply(function(x) predict(PT_glm, x, type = 'response', se.fit = T)) %>%
  bind_rows() %>%
  # Rename column and add column for days and environmental conditions
  select(! residual.scale) %>%
  mutate(days = 0:30, conditions = 'lowT_lowP', ci = se.fit*1.96)

# For high precipitation and temp
preds_highPT <- days_df %>%
  mutate(Precipitation = p_high, Temperature = t_high) %>%
  group_split() %>%
  # Predict from days 0-30
  lapply(function(x) predict(PT_glm, x, type = 'response', se.fit = T)) %>%
  bind_rows() %>%
  # Rename column and add column for days and environmental conditions
  select(! residual.scale) %>%
  mutate(days = 0:30, conditions = 'highT_highP', ci = se.fit*1.96)

# For low precipitation and high temp
preds_lowP_highT <- days_df %>%
  mutate(Precipitation = p_low, Temperature = t_high) %>%
  group_split() %>%
  # Predict from days 0-30
  lapply(function(x) predict(PT_glm, x, type = 'response', se.fit = T)) %>%
  bind_rows() %>%
  # Rename column and add column for days and environmental conditions
  select(! residual.scale) %>%
  mutate(days = 0:30, conditions = 'highT_lowP', ci = se.fit*1.96)

# For high precipitation and low temp
preds_highP_lowT <- days_df %>%
  mutate(Precipitation = p_high, Temperature = t_low) %>%
  group_split() %>%
  # Predict from days 0-30
  lapply(function(x) predict(PT_glm, x, type = 'response', se.fit = T)) %>%
  bind_rows() %>%
  # Rename column and add column for days and environmental conditions
  select(! residual.scale) %>%
  mutate(days = 0:30, conditions = 'lowT_highP', ci = se.fit*1.96)

# Bind predictions together
preds_df <- bind_rows(preds_lowPT, preds_highPT, 
                      preds_highP_lowT, preds_lowP_highT) %>%
  mutate(N_present = fit * 100, N_ci = ci * 100)

# Make legend labels
l_labs <- c('High temp., high precip.', 'High temp., low precip.',
            'Low temp., high precip.', 'Low temp., low precip.')

# Plot
p <- preds_df %>%
  ggplot(aes(x = days, y = N_present, colour = conditions)) +
  scale_colour_manual(values = c('#ff4a4a', '#FFC56C', '#2150fb', '#003A6F'),
                      labels = l_labs) +
  scale_fill_manual(values = c('#ff4a4a', '#FFC56C', '#2150fb', '#003A6F'),
                    labels = l_labs) +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_ribbon(aes(x = days, fill = conditions,
                  ymin = N_present - N_ci, 
                  ymax = N_present + N_ci), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 1) +
    theme(panel.background=element_rect(fill='white', colour='black'), 
          axis.text=element_text(size=15, colour='black'),
          axis.title.y=element_text(size=15, colour='black', vjust=4), 
          axis.title.x=element_text(size=15, colour='black', vjust=-2),
          panel.grid=element_blank(), plot.margin=unit(c(0.5,0.5,1,1),  'cm'),
          legend.position=c(.25,.2), 
          legend.title=element_blank(), legend.key.size=unit(.75, 'cm'),
          legend.key=element_rect(fill=NA, colour=NA), 
          legend.text=element_text(colour='black', size=12)) +
  labs(x = 'Survey frequency (days)', y = 'Estimated mortality')

# Save the plot
saveRDS(p, 'output/est_mort_plot.rds')

