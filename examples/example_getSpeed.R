require(TuktuTools)

data(caribou)

# keep data during calving period (i.e., May 19 to July 7), no more than 3 consecutive days of missing data
# and at least 1 fix per day
b <- cutperiod(caribou, start = c(05,19), end = c(07,07), nfixes = 1, dayloss = 3)

# get movement rate
c <- getSpeed(b) %>% mutate(Year = year(Time))

# visualize movement rate

ggplot2::ggplot(data=c, aes(x=time.mid, y = speed, group = ID, color = ID)) + geom_path() + 
  facet_wrap(~Year, scales = "free")
