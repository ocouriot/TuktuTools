#' Calculate Daily Mean
#' 
#' 
#' 
#' @param x a data frame (or simple feature) with the ID, the Time (as date and time), the x and y coordinates in metric system, 
#' and the Lon and Lat coordinates in WGS84
#' @param idcol character string of the name of the ID column 
#' @param timecol character string of the name of the time column
#' @return A data frame (or simple feature) with the daily mean locations
#' 
#' @example examples/example_getDailyMean.R
#' 
#' @export

# getDailyMean <- function(df){
#   df_mean <- df %>% as.data.frame %>%
#     plyr::mutate(yday = lubridate::yday(Time), Year = year(Time)) %>%
#     group_by(ID, Year, yday, .add = TRUE) %>%
#     summarize(x = ifelse(is.null(x), NA, mean(x, na.rm = TRUE)), y = ifelse(is.null(y), NA, mean(y, na.rm = TRUE)), Time = mean(Time),
#               Lon = ifelse(is.null(Lon), NA, mean(Lon, na.rm = TRUE)), Lat = ifelse(is.null(Lat), NA, mean(Lat, na.rm = TRUE))) %>%
#     ungroup %>% as.data.frame
#   df_mean
# }


getDailyMean <- function(x, idcol = "ID", timecol = "Time", ...){
  
  if(inherits(x, "sf")){
    x.sf <- x
    x$X <- st_coordinates(x.sf)[,1]
    x$Y <- st_coordinates(x.sf)[,2]
  } else x.sf <- NULL
  
  
  x_dailymean <- x %>% as.data.frame %>%
    plyr::mutate(yday = lubridate::yday(get(timecol)), Year = year(get(timecol))) %>%
    group_by(get(idcol), Year, yday, .add = TRUE) %>%
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
              across(where(is.factor), ~ unique(.x)),
              across(where(is.character), ~ unique(.x)),
              across(where(is.POSIXct), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup %>% as.data.frame
  
  names(x_dailymean)[names(x_dailymean) == "get(idcol)"] <- idcol
  
  if(!is.null(x.sf)) 
    x_dailymean <- st_as_sf(x_dailymean, coords = c("X","Y"),  crs = st_crs(x.sf))
  x_dailymean
}
