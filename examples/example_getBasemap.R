require(moveVis)
require(TuktuTools)
require(slippymath)
require(pbapply)
require(curl)
require(magick)

data("bathurst_lines")
b <- bathurst_lines

CRS2 = "+init=epsg:4326"

gg.ext <- st_bbox(b)

dir.create("basemap")

a <- getBasemap(gg.ext = st_bbox(b), map_service = "osm", map_type = "terrain_bg", 
                map_dir ="basemap", map_res = .5, m.crs = 4326)

plotRGB(a[[1]])
plot(st_geometry(b %>% st_transform(crs(a[[1]]))), add = TRUE, col = 1:nrow(b))
