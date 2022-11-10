require(moveVis)
require(TuktuTools)
require(slippymath)
require(pbapply)
require(curl)
require(magick)

data("caribou")
b <- caribou %>% st_as_sf(coords=c("Lon", "Lat"), crs = 4326)

gg.ext <- st_bbox(b)
dir.create("basemap")

a <- getBasemapRaster(gg.ext)

b_lines <- b %>% group_by(ID, Year) %>%
  summarize(do_union=FALSE) %>% st_cast("LINESTRING")

plotRGB(a)
plot(st_geometry(b_lines %>% st_transform(crs(a[[1]]))), add = TRUE, col = 1:nrow(b), cex = 2)
