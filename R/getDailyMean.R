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

# getDailyMean <- function(df){
#   df_mean <- df %>% as.data.frame %>%
#     plyr::mutate(yday = lubridate::yday(Time), Year = year(Time)) %>%
#     group_by(ID, Year, yday, .add = TRUE) %>%
#     summarize(x = ifelse(is.null(x), NA, mean(x, na.rm = TRUE)), y = ifelse(is.null(y), NA, mean(y, na.rm = TRUE)), Time = mean(Time),
#               Lon = ifelse(is.null(Lon), NA, mean(Lon, na.rm = TRUE)), Lat = ifelse(is.null(Lat), NA, mean(Lat, na.rm = TRUE))) %>%
#     ungroup %>% as.data.frame
#   df_mean
# }


getDailyMean <- function(df){
  df_mean <- df %>% as.data.frame %>%
    plyr::mutate(yday = lubridate::yday(Time), Year = year(Time)) %>%
    group_by(ID, Year, yday, .add = TRUE) %>%
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
              across(where(is.factor), ~ unique(.x)),
              across(where(is.character), ~ unique(.x)),
              across(where(is.POSIXct), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup %>% as.data.frame
  df_mean
}
