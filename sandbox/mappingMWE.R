require(moveVis)
require(TuktuTools)
require(slippymath)
require(pbapply)
require(curl)
require(magick)

data("bathurst_lines")
source("getMap.R")
b <- bathurst_lines

CRS2 = "+init=epsg:4326"

gg.ext <- st_bbox(b)

map_service <- "osm"
map_type <- "terrain_bg"
map_dir <- "../sandbox/basemap/"
m.crs <- CRS2
map_res <- .5

a <- getBasemap(gg.ext, map_service, map_type, 
                map_token, map_dir, map_res, m.crs)


plotRGB(a[[1]])
plot(st_geometry(b %>% st_transform(crs(a[[1]]))), add = TRUE, col = 1:nrow(b))




