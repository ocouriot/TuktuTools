#' get speed
#'
#' This function calculate speed from raw movement data containing x and y coordinates (in meters) with corresponding date and time
#' in POSIXct format. This will calculate the speed between subsequent relocations (i.e. of each step), as well as the middle time
#' of the step.
#'
#' @param  df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (in N Canada Lambert Conformal Conic)
#' Time: date and time vector (of class POSIXct)
#'@param CRS the coordinates projection (default is Canada Lambert Conformal Conic:
#' "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#' @return The function returns the same dataframe than 'df' with columns speed, which is the speed between subsequent relocations (in m.h-1),
#' time.mid corresponding to the average time of subsequent relocations,
#' dhours which is an index of the cumulative hours of each speed values from the first speed value,
#' dt the time lags between subsequent relocations (in hour)
#'
#' @example examples/example_getSpeed.R
#' 
#' @export
#' 

getSpeed <- function(x){
  # 
  df_annotated <- sf %>% as.data.frame %>% ddply("ID","Year", annotateWithSpeeds)
  # recreate simple feature
}

annotateWithSpeeds <- function(df){
  droplevels(df) %>% arrange(Time) %>% 
    mutate(z = x+1i*y, 
           dhours = c(NA, difftime(Time[-1], Time[-length(Time)], units = "hours") %>% as.numeric), 
           sl = c(NA,Mod(diff(z))), 
           speed = sl/dhours,
           time.mid = (Time[-1] + Time[-length(Time)])/2)
}  
