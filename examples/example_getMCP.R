require(TuktuTools)
data(caribou)

# Winter (December 1 to March 15)
winterpoints <- subset(caribou, (month(Time) == 12 & Year == 2003) |
                         (month(Time) %in% c(1,2) & Year == 2004) |
                         (mday(Time) < 16 & month(Time) == 2 & Year == 2004)) %>% droplevels

# aggregate to keep one point per week to avoid autocorrelation
winterpoints <- winterpoints %>% mutate(week = week(Time)) %>% group_by(week) %>%
  summarize(x = mean(x), y = mean(y), ID = unique(ID)) %>% ungroup %>% as.data.frame

# convert to simple features in metric system
CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
winter.sf <- st_as_sf(winterpoints, coords = c("x","y"), crs=CRS)

# estimate MCP 95% and 50%
mcp95 <- getMCP(winter.sf)
mcp50 <- getMCP(winter.sf, percent = 50)

# Visualize ranging areas
require(mapview)

# Minimum convex polygon
mapview(winter.sf) +
  mapview(mcp95, col.regions = "red") +
  mapview(mcp50, col.regions = "darkred")
