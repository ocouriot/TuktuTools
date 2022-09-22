#' Calculate Daily Mean
#' 
#' @param mysf a simple feature subseted to a single ID.  
#' @return A simple feature with the same CRS containing daily mean locations. 


getDailyMean <- function(mysf){
  df_mean <- mysf %>% mutate(x = st_coordinates(mysf)[,1],
                             y = st_coordinates(mysf)[,2])  %>% 
    data.frame %>% droplevels %>%
    plyr::mutate(yday = lubridate::yday(DateTime)) %>%
    group_by(Year, yday, add = TRUE) %>%
    summarize(x = mean(x), y = mean(y), DateTime = mean(DateTime))
  
  sf_mean <- st_as_sf(df_mean, coords = c("x","y"), crs = st_crs(mysf)) %>%
    plyr::mutate(x = df_mean$x, y = df_mean$y)
  sf_mean
}


