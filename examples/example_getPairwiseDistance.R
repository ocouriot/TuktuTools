data(barrenground)

b.subset <- barrenground %>% plyr::mutate(yday = yday(DateTime)) %>% 
  arrange(ID, DateTime) %>% subset(Year == 2017) %>%
  st_as_sf(coords = c("Lon","Lat")) %>% st_set_crs(4326)

b.list <- dlply(b.subset, "ID", st_as_sf)

distance.df <- getPairwiseDistances(b.list)

# plotting
require(gplots)
palette(rich.colors(length(b.list)))
scan_tracks(b.list, plotdistance = TRUE, distance.df = distance.df, legend = TRUE)

# plotting median distances over time

boxplot(I(distance/1000)~yday, data = distance.df,
        at = sort(unique(distance.df$yday)),
        pch = 19, cex = 0.3, lty = 1,
        col = "grey", border = "darkgrey", varwidth = TRUE)

# all proximities less than 50 m
subset(distance.df, distance < 200)

# list of "social" individuals
socialanimals <- with(subset(distance.df, distance < 200), unique(c(ID1, ID2)))
socialpairs <- with(subset(distance.df, distance < 200), unique(pair))

n.pairs <- length(socialpairs)
cols <- rich.colors(n.pairs)

distance.sps <- distance.df %>% subset(pair %in% socialpairs)
plot(distance~Date, data = distance.sps, type = "n")
d_ply(distance.sps, "pair",
      function(df) lines(df[,c("Date","distance")],
                         col = cols[match(df$pair[1], socialpairs)] %>% alpha(0.5)))

with(distance.sps %>% subset(distance < 50),
     points(Date, jitter(distance),
     col = cols[match(pair, socialpairs)], pch = 19))