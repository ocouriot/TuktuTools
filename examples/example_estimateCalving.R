require(TuktuTools)

data(caribou)

# remove potential outliers and keep only females
caribou.cleaned <- removeOutliers(caribou) %>% subset(outlier == "FALSE" & sex == "f")

# keep data during calving period (i.e., May 19 to July 7), no more than 3 consecutive days of missing data
# and at least 1 fix per day
caribou.prepped <- prepData(caribou.cleaned, start = "05-19", end = "07-07", 
                            nfixes = 1, dayloss = 3)

# get movement rate for the females
caribou.mr <- getSpeed(caribou.prepped)
head(caribou.mr)

# estimate calving with the no calf and calf model
parturitions <- estimateCalving(caribou.mr, int=3, kcons =c(5,21), models = "full", drawplot = TRUE)
head(parturitions)

# get plot for one individual
table(caribou.mr$ID, caribou.mr$Year) # take Vixen in 2008

# what is the identified model for Vixen in 2008?
parturitions$results[parturitions$results$ID == "Vixen" & 
                       parturitions$results$Year == 2008,] # "calfdeath"

plotCalvingRange(df = caribou.mr[caribou.mr$ID == "Vixen" & 
                                   caribou.mr$Year == 2008,], int = 3,
               kcons = c(5,21), bestmodel = "calfdeath")
