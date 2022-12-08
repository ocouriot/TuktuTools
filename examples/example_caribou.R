data(caribou)
head(caribou)

# plotting locations on a base graphics map
require(mapdata)
maps::map("worldHires", xlim = c(-135,-120), ylim=c(62,67), col = "grey")
ids <- levels(caribou$ID)
for(i in 1:length(ids))
  with(caribou %>% subset(ID == ids[i]), lines(Lon, Lat, col = i))

# plotting points with mapview
caribou.sf <- st_as_sf(caribou %>% mutate(Lat = Lat, Lon = Lon), 
                       coords = c("Lon","Lat")) %>% st_set_crs(4326)
mapview::mapview(caribou.sf[,"ID"])

# converting to lines
lines <- caribou.sf %>% group_by(ID) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING")
mapview::mapview(lines[,"ID"])
