require(TuktuTools)
a <- getWeatherAtLocation(-152.5, 68.25, 
                          start = 2010, end = 2022)

# look at stations
data("gsn_stations")
gsn_stations %>% head
gsn_stations %>% subset(station %in% a$Station)

# plot avg temperature by year
require(ggplot2)

a %>% subset(Month == 7,) %>% arrange(Day) %>% 
  ggplot(aes(Day, Tavg, col = Station)) + 
  geom_ribbon(aes(ymin = Tmin, ymax = Tmax)) + 
  geom_point() + geom_path() + facet_wrap(.~Year)

# map stations in Alaska region where lon < -130 & lat > 60
require(sf)
gsn.sf <- st_as_sf(gsn_stations %>% subset(lon < -130 & lat > 60), 
                   coords = c("lon", "lat")) %>% st_set_crs(4326)

mapview::mapview(gsn.sf[,"station"])
