require(TuktuTools)

data(caribou)

# keep data during calving period (i.e., May 19 to July 7), no more than 3 consecutive days of missing data
# and at least 1 fix per day
b <- cutperiod(caribou, start = c(05,19), end = c(07,07), nfixes = 1, dayloss = 3)

# get daily rate for the females
c <- getSpeed(b)

# run the parturition model with the no calf and calf model
parturitions <- parturition.model2(c, int=3, kcons =c(5,21), PlotIt = TRUE)
