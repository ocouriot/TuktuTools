#' Daily Distance Calculations
#'
#' This function (rather quickly) computes pairwise distances between daily mean locations
#' of animals.
#'
#' @param sf.list list of simple feature tracks
#'
#'
#' @return data frame with (among others) ID, nickname, distance, date
#' @seealso
#'
#' @examples
#' data(bathurst)
#' b.subset <- bathurst %>% plyr::mutate(yday = yday(DateTime)) %>%
#'         subset(Year == 2014)
#' b.list <- dlply(b.subset, "ID", st_as_sf)
#' distance.df <- getPairwiseDistances(b.list)
#'
#' # plotting
#' require(gplots)
#' palette(rich.colors(length(b.list)))
#' scan_tracks(b.list, plotdistance = TRUE, distance.df = distance.df, legend = TRUE)
#'
#' # plotting median distances over time
#'
#' boxplot(I(distance/1000)~yday, data = distance.df,
#'         at = sort(unique(distance.df$yday)),
#'         pch = 19, cex = 0.3, lty = 1,
#'         col = "grey", border = "darkgrey", varwidth = TRUE)
#'
#' # all proximities less than 50 m
#' subset(distance.df, distance < 50)
#'
#' # list of "social" individuals
#' socialanimals <- with(subset(distance.df, distance < 50), unique(c(ID1, ID2)))
#' socialpairs <- with(subset(distance.df, distance < 50), unique(pair))
#'
#' n.pairs <- length(socialpairs)
#' cols <- rich.colors(n.pairs)
#'
#' distance.sps <- distance.df %>% subset(pair %in% socialpairs)
#' plot(distance~Date, data = distance.sps, type = "n")
#' d_ply(distance.sps, "pair",
#'       function(df) lines(df[,c("Date","distance")],
#'                          col = cols[match(df$pair[1], socialpairs)] %>% alpha(0.5)))
#'
#' with(distance.sps %>% subset(distance < 50),
#'      points(Date, jitter(distance),
#'      col = cols[match(pair, socialpairs)], pch = 19))


getPairwiseDistances <- function(sf.list){

  getDistances <- function(sf){
    D <- st_distance(sf)
    pair.M <- outer(sf$ID, sf$ID, paste)
    pair <- pair.M[upper.tri(D)]

    ID1 <- sapply(strsplit(pair, " "), function(x) x[1])
    ID2 <- sapply(strsplit(pair, " "), function(x) x[2])

    data.frame(ID1, ID2, pair, distance = D[upper.tri(D)])
  }

  sf <- sf.list %>% ldply %>% st_as_sf %>% getDailyMean

  #cat("Computing all pairwise distances\n")
  sf %>% group_by(Year, yday) %>% group_modify(~getDistances(.x), keep = TRUE) %>%
    mutate(Date = (ymd_hm(paste(Year - 1, "12-31 12:00")) + ddays(yday)) %>% as.POSIXct)
}

getDailyMean <- function(mysf){

    # need to group by ALL factors OR characters ... (as well as Year and yday)
    groupingrule <- function(x) is.factor(x) | is.character(x)

    df_mean <- mysf %>% data.frame %>% droplevels %>%
      plyr::mutate(yday = lubridate::yday(DateTime)) %>%
      group_by_if(groupingrule) %>%
      group_by(Year, yday, add = TRUE) %>%
      summarize(x = mean(x), y = mean(y), Lon = mean(Lon), Lat = mean(Lat),
                DateTime = mean(DateTime))
    sf_mean <- st_as_sf(df_mean, coords = c("x","y"), crs = st_crs(mysf)) %>%
      plyr::mutate(x = df_mean$x, y = df_mean$y)
    sf_mean
}


