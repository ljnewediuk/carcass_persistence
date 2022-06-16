

### Load libraries ===
libs <- c('data.table', 'tidyverse', 'lubridate')
lapply(libs, require, character.only=T)

### Load data ===
dat <- fread('input/Frogs.csv')

### Organize ===
# Make datetime POSIX
dat[, c('DF', 'DL') := .(as.POSIXct(DF, format='%Y-%m-%d'), as.POSIXct(DL, format='%Y-%m-%d'))]
# Add julian day and month cols
dat[, c('JDayF', 'MonthF', 'JDayL', 'MonthL') := .(yday(DF), month(DF), yday(DL), month(DL))]
# Convert MDTV to thousands/day
dat[, 'MDTV' := .(MDTV/1000)]
# Convert precipitation to average
dat[, 'Precipitation' := .(Precipitation/Days)]

### Save data ===
saveRDS(dat, 'input/Frogs_clean.rds')

### Data exploration ===

# Plot SVL by species
ggplot(dat, aes(x=SVL, colour=Species)) + geom_density()

# Plot temperature by precipitation
ggplot(dat, aes(x=Precipitation, y=Temperature)) + geom_point() +  geom_smooth(method='loess')

# Plot continuous predictor variables by julian day
# Quadratic relationship between temp/precipitation and julian day?
dat_mlt <- melt(dat, measure.vars=c('Precipitation', 'Temperature', 'MDTV', 'SVL'))
ggplot(dat_mlt, aes(x=JDayF, y=value)) + geom_point() + geom_smooth(method=loess) + facet_wrap(~variable, scales='free')

# Plot counts and species by julian day
# Create table for species counts/day
spp_by_day <- data.table()
for(i in unique(dat$JDayF)){
  jday_row <- dat[JDayF==i]
  BULL <- nrow(jday_row[Species=='BULL'])
  TREE <- nrow(jday_row[Species=='TREE'])
  TOAD <- nrow(jday_row[Species=='TOAD'])
  NLFR <- nrow(jday_row[Species=='NLFR'])
  GREN <- nrow(jday_row[Species=='GREN'])
  WOOD <- nrow(jday_row[Species=='WOOD'])
  spp_day <- data.table(JDayF=i, Month=mean(jday_row$MonthF), BULL, TREE, TOAD, NLFR, GREN, WOOD)
  spp_by_day <- rbind(spp_by_day, spp_day)
}
# Melt
spp_by_day <- melt(spp_by_day, measure.vars=c('BULL', 'TREE', 'TOAD', 'NLFR', 'GREN', 'WOOD'), variable.name='Species', value.name='Count')
# Density plots by species
ggplot(spp_by_day, aes(x=Count, colour=factor(Month))) + geom_density() + facet_wrap(~Species)
# Counts over time by species
ggplot(spp_by_day, aes(x=JDayF, y=Count)) + geom_point() + geom_smooth(method='loess') + facet_wrap(~Species)
# Counts over time
counts <- spp_by_day[, list(Count=sum(Count)), by='JDayF']
ggplot(counts, aes(x=JDayF, y=Count)) + geom_point() + geom_smooth(method='lm')






