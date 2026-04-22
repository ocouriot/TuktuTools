require(TuktuTools)

data(caribou)
caribou_dailymean <- getDailyMean(caribou)
head(caribou_dailymean)


# For big datasets, use getDailyMean_dt

require(TuktuData)
data(nwt_raw)
bathurst <- subset(nwt_raw, study_site == "Bathurst")
nrow(bathurst)
system.time(
    bathurst_dailymean <- getDailyMean_dt(bathurst, id.col = "OriginalID")
)
