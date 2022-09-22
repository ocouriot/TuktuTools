require(moveVis)
require(TuktuTools)
require(slippymath)
require(pbapply)
require(curl)
require(magick)

data("bathurst_lines")
b <- bathurst_lines

gg.ext <- st_bbox(b)
dir.create("basemap")

a <- getBasemap(gg.ext = st_bbox(b), map_service = "osm", map_type = "terrain_bg", 
                map_dir ="basemap", map_res = .5, m.crs = st_crs(b))

plotRGB(a)
plot(st_geometry(b %>% st_transform(crs(a[[1]]))), add = TRUE, col = 1:nrow(b))
