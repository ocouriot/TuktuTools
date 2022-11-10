require(moveVis)
require(TuktuTools)
require(slippymath)
require(pbapply)
require(curl)
require(magick)

data("caribou")
b <- caribou %>% group_by(ID) %>% st_as_sf(coords=c("Lon", "Lat"), crs=4326) %>%
  summarize(do_union=FALSE) %>% st_cast("LINESTRING")

gg.ext <- st_bbox(b)
dir.create("basemap")

a <- getBasemap(gg.ext = st_bbox(b), map_service = "osm", map_type = "terrain_bg", 
                map_dir ="basemap", map_res = .5, m.crs = st_crs(b))

plotRGB(a)
plot(st_geometry(b %>% st_transform(crs(a[[1]]))), add = TRUE, col = 1:nrow(b))
