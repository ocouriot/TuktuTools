require(TuktuTools)

data(caribou)

# keep data during calving period (i.e., May 19 to July 7), no more than 3 consecutive days of missing data
# and at least 1 fix per day
prepped.caribou <- prepData(caribou.cleaned, start = "05-19", end = "07-07", nfixes = 1, dayloss = 3)
