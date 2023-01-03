require(TuktuTools)
data(simulated_migrations)
# contains a data frame (simulated_migrations) and a POINTS simple features
# (simulated_migrations.sf)

ids <- levels(unique(simulated_migrations$ID))
n.ids <- length(ids)

require(viridis)
palette(viridis(n.ids))
scan_tracks(simulated_migrations.sf, legend = TRUE)
scan_tracks(simulated_migrations %>% mutate(Time = yday(Time)))


x <- bathurst_clipped %>% subset(Year == 2018)
  
scan_tracks()