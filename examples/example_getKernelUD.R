data("barrenground")

b <- subset(barrenground, ID == ID[1]) %>% arrange(DateTime)
with(b, plot(x,y, type = "o"))

# convert to simple feature

b.sf <- st_as_sf(b, coords = c("Lon","Lat")) %>% st_set_crs(4326)
b.ud.95 <- getKernelUD(b.sf)
b.ud.50 <- getKernelUD(b.sf, 50)
m1 <- mapview(b.sf)
m2 <- mapview(b.ud.95, map = m1@map, col.regions = "red")
mapview(b.ud.50, map = m2@map, col.regions = "red")
