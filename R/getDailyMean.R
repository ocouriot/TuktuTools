#' Calculate Daily Mean
#' 
#' Important note - requires a column called "DateTime"
#' 
#' @param df a data frame with the ID, the Time (as date and time), the x and y coordinates in metric system, 
#' and the Lon and Lat coordinates in WGS84
#' @return A data frame with the daily mean locations
#' 
#' @example examples/example_getDailyMean.R
#' 
#' @export

getDailyMean <- function(df){
  df_mean <- df %>%
    plyr::mutate(yday = lubridate::yday(Time), Year = year(Time)) %>%
    group_by(ID, Year, yday, add = TRUE) %>%
    summarize(x = ifelse(is.null(x), NA, mean(x)), y = ifelse(is.null(y), NA, mean(y)), Time = mean(Time),
              Lon = ifelse(is.null(Lon), NA, mean(Lon)), Lat = ifelse(is.null(Lat), NA, mean(Lat)))
  
  df_mean
}