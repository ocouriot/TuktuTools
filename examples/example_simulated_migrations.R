# load the data
data(simulated_migrations)

# use scan tracks to plot
scan_tracks(simulated_migrations, legend = TRUE)

# nicer version with the simple feature
simulated_migrations.sf %>% 
  scan_tracks(id.col = "Name", colors = rainbow(18),
              legend= TRUE, legend.pos = "topleft")

# use mapview to see the tracks in projection
require(mapview)
simulated_migrations.sf %>% group_by(Name) %>% 
  summarize(do_union=FALSE) %>% st_cast("LINESTRING") %>% 
  mapview
