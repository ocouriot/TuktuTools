require(TuktuTools)

data(caribou)
CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

head(caribou)

# for this example, we will take all the locations of the animals at a given day, 
# whatever the year, to have enough locations for this day.
# However, this method has been developped to estimate daily ranges for animals 
# monitored the same year.

# add the day of year
caribou <- caribou %>% mutate(yday = yday(Time))

# Take one day during winter and one day during summer to compare dispersion during those two seasons
days <- caribou %>% subset((mday(Time) == 15 & month(Time) == 7) |
                             (mday(Time) == 15 & month(Time) == 1))


# get average daily location for each individual and year then assign the same year for computing the daily range
days <- days %>% getDailyMean %>% st_as_sf(coords=c("x","y"), crs = CRS) %>% 
  mutate(Year = 2015, Date = as.character(as.Date(paste0("2015", substr(Time, 5, 10))))) %>% mutate(yday = yday(Date))

# daily LoCoH
require(tlocoh); require(raster)
dailyranges.locoh <- getDailyRange(days, method = "LoCoH", nn = 5, level = .95, crs = CRS) %>%
  mutate(Date = as.Date(yday, origin = "2014-12-31") %>% as.character)

# daily KernelUD
require(adehabitatHR)
dailyranges.ud <- getDailyRange(days, method = "KernelUD", percent = 95) %>%
  mutate(Date = as.Date(yday, origin = "2014-12-31") %>% as.character)

# visualize resulting daily ranges
map <- mapview::mapview(days, zcol = "Date")
m2 <- mapview::mapview(dailyranges.ud, map = map@map, zcol = "Date")
mapview::mapview(dailyranges.locoh, map = m2@map, zcol = "Date")
