#' get speed
#'
#' This function calculate speed from raw movement data containing x and y 
#' coordinates (in meters) with corresponding date and time
#' in POSIXct format. This will calculate the speed between subsequent 
#' relocations (i.e. of each step), as well as the middle time
#' of the step.
#'
#' @param  x a data frame containing columns: ID as individual identifiant,
#' x and y coordinates (in a metric system)
#' Time: date and time vector (of class POSIXct)
#' @return The function returns the same dataframe than 'x' with columns `speed`, 
#' which is the movement rate between subsequent relocation (in m.h-1), `sl` 
#' which is the step length between subsequent locations, `dhours` which is an 
#' index of the cumulative time (in hour) from the first to the nth relocation, 
#' `dt` the time lag between subsequent relocation (in hour).
#'
#' @example examples/example_getSpeed.R
#' 
#' @export
#' 

getSpeed <- function(x){
  
  if(class(x)[1] == "data.frame"){
    df_annoted <- x %>% as.data.frame %>% ddply(c("ID"), annotateWithSpeeds)
  }
  if(class(x)[1] == "sf"){
    CRS = st_crs(x)
    df_annoted <- x %>% as.data.frame %>% ddply(c("ID"), annotateWithSpeeds) %>% st_as_sf(crs = CRS)
  }
  return(df_annoted)
}

annotateWithSpeeds <- function(df){
  droplevels(df) %>% dplyr::arrange(Time) %>% 
    mutate(z = x+1i*y, 
           dt = c(NA, difftime(Time[-1], Time[-length(Time)], units = "hours") %>% as.numeric), 
           sl = c(NA, Mod(diff(z))), 
           dhours = difftime(Time, Time[1], units = "hours") %>% as.numeric,
           speed = sl/dt)
}  
