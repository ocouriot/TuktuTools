require(TuktuTools)

data(simulated_migrations)

head(simulated_migrations)

# for this example, we will take all the locations of the animals at a given day, 
# whatever the year, to have enough locations for this day.
# However, this method has been developed to estimate daily ranges for animals 
# monitored the same year.

# add the day of year
migrants <- simulated_migrations.sf

# get average daily location for each individual and year then assign the same year for computing the daily range
migrants.dailymean <- simulated_migrations.sf %>% getDailyMean

# daily KernelUD
require(adehabitatHR)
dailyranges.ud <- getDailyRange(migrants.dailymean, method = "KernelUD", 
                                percent = 95) %>%
  mutate(Date = as.Date(yday, origin = "2014-12-31"))

mapview::mapview(st_as_sf(dailyranges.ud), zcol = "yday")
with(dailyranges.ud, plot(Date, area, type = "l"))

