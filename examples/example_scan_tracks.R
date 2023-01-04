require(TuktuTools)
data(simulated_migrations)

# latitude longitude
scan_tracks(simulated_migrations, legend  = TRUE)

# Use the simple feature version, with names in the legend
simulated_migrations.sf %>% 
  scan_tracks(id.col = "Name", colors = rainbow(18),
              legend= TRUE, legend.pos = "topleft")

