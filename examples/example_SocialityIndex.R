library(TuktuData)
library(TuktuTools)

data(nwt_raw)
b2023 <- subset(nwt_raw,
                Year == 2023 & study_site == "Bathurst" &
                    lubridate::month(Time) %in% 5:9)

## 1. Daily mean locations
b2023_dm <- getDailyMean_dt(b2023, id.col = "OriginalID", time.col = "Time")

## 2. Sociality index (daily mean already computed)
system.time(si <- getSocialityIndex(b2023_dm, r = 200, percent = 95))

## 3. In parallel (much faster)

plan(multisession, workers = 10)
system.time(si <- getSocialityIndex(b2023_dm, r = 200, parallel = TRUE, percent = 50))
head(si)

## Plot
with(si, {
    par(mfrow = c(2,2), bty = "l", mar = c(3,4,2,1), mgp = c(2,.5,0), tck = 0.02)
    plot(yday, n,      type = "o", pch = 19, ylab = "n", main = "N. individuals")
    plot(yday, A_km2,  type = "o", pch = 19, ylab = expression(km^2),
         main = "95% kernel area")
    plot(yday, N_enc,  type = "h", lwd = 2, ylab = "N_enc",
         main = "Observed encounters (<200 m)")
    plot(yday, SI + 1, type = "o", pch = 19, log = "y",
         ylab = "SI + 1", main = "Sociality Index")
})

