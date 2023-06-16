require(TuktuTools)
require(vctrs)
require(mapview)

data(caribou)

# convert to sf object and add a herd assignment
caribou.sf <- st_as_sf(caribou, 
                       coords = c("Lon","Lat"), crs = 4326) %>%
  mutate(Herd = "Santa")

mapview(caribou.sf)

# create multipolygon sf for boundaries from mcps
mcp50 <- getMCP(caribou.sf, 50)
mcp95 <- getMCP(caribou.sf)

mcps <- rbind(mcp50, mcp95)

# visualize points and mcps
mapview(mcps) + mapview(caribou.sf, zcol = "ID")

# create summary of points in each polygon boundary
pts_summary <- countPtsInPolygons(caribou.sf, mcps, id_col =  "id", assigned_col = "Herd")
