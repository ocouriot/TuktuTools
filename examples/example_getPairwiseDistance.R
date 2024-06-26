data(caribou)

# subset on the two individuals which have monitoring the same year (Vixen and Comet, in 2007)
# and transform into Simple Feature object
b.subset <- caribou %>% plyr::mutate(yday = yday(Time)) %>% 
  arrange(ID, Time) %>% subset(Year == 2007) %>%
  st_as_sf(coords = c("Lon","Lat")) %>% st_set_crs(4326) %>%
  mutate(Lon = st_coordinates(.)[,1], Lat = st_coordinates(.)[,2])

b.list <- dlply(b.subset, "ID", st_as_sf) # create a list of the sf by individual

# estimate the pairwise distances
distance.df <- getPairwiseDistances(b.list)

# Figure of the distance between the two individuals over time
plot(distance/1000~Date, data = distance.df, type = "l", ylab = "Distance between individuals (km)",
     xlab ="Date")

# Figure of the distance between the two individuals with the moments they were less than 5km apart
plot(distance/1000~Date, data = distance.df, type = "l", ylab = "Distance between individuals (km)",
     xlab ="Date")
points(distance/1000~Date, data = distance.df[distance.df$distance<5000,], pch = 19, col = "red")

