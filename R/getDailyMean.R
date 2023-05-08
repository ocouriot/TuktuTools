#' Calculate Daily Mean
#' 
#' This function takes any large movement dataset and return the daily mean locations 
#' for each individual for each day for each year of observations. 
#' 
#' @details Every continuous variable (X, Y, Lon, Lat, Time) will be averaged for each day.  Every factor 
#' or character vector will be reduced to a single (first) observation.
#' 
#' @param x a data frame or simple feature.  
#' @param id.col character string of the name of the ID column 
#' @param time.col character string of the name of the time column
#' @return A data frame (or simple feature) with the daily mean locations, the day of year (yday), 
#' the year, and otherwise all continuous variables averaged, and all discrete data reduced to the 
#' first observation. A data frame will return a data frame, a simple feature will return a simple 
#' feature conserving the original projection. 
#' 
#' @example examples/example_getDailyMean.R
#' 
#' @export

getDailyMean <- function(x, id.col = "ID", time.col = "Time", ...){
  
  if(inherits(x, "sf")){
    x.sf <- x
    x$X <- st_coordinates(x.sf)[,1]
    x$Y <- st_coordinates(x.sf)[,2]
  } else x.sf <- NULL

  x_dailymean <- x %>% as.data.frame %>%
    dplyr::mutate(yday = lubridate::yday(get(time.col)), 
                 Year = lubridate::year(get(time.col)),
                 ID = get(id.col)) %>%
    group_by(ID, Year, yday, .add = TRUE) %>%
    dplyr::summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
              across(where(is.factor), ~ head(.x,1)),
              across(where(is.character), ~ head(.x,1)),
              across(where(is.POSIXct), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup %>% as.data.frame
  
  if(id.col != "ID") x_dailymean$ID <- NULL 
  
  if(!is.null(x.sf)) 
    x_dailymean <- st_as_sf(x_dailymean, coords = c("X","Y"),  crs = st_crs(x.sf))
  x_dailymean
}
