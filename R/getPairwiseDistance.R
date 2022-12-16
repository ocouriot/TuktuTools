#' Daily Distance Calculations
#'
#' This function (rather quickly) computes pairwise distances between daily mean locations
#' of animals.
#'
#' @param sf.list list of simple feature tracks containing the ID
#'
#'
#' @return data frame with (among others) ID, nickname, distance, date
#'
#' @example examples/example_getPairwiseDistance.R
#' 
#' @export


getPairwiseDistances <- function(sf.list){

  getDistances <- function(sf){
    D <- st_distance(sf)
    pair.M <- outer(sf$ID, sf$ID, paste)
    pair <- pair.M[upper.tri(D)]

    ID1 <- sapply(strsplit(pair, " "), function(x) x[1])
    ID2 <- sapply(strsplit(pair, " "), function(x) x[2])

    data.frame(ID1, ID2, pair, distance = as.numeric(D[upper.tri(D)]))
  }

  sf <- sf.list %>% ldply(getDailyMean) %>% st_as_sf

  #cat("Computing all pairwise distances\n")
  sf %>% group_by(Year, yday) %>% group_modify(~getDistances(.x), .keep = TRUE) %>%
    mutate(Date = (ymd_hm(paste(Year - 1, "12-31 12:00")) + ddays(yday)) %>% as.POSIXct)
}
