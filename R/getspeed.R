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

getSpeed <- function(x, id.col = "ID", x.col = "x", y.col = "y", ...){
  
  if(class(x)[1] == "data.frame"){
    df_annotated <- x %>% mutate(X = get(x.col), Y = get(y.col)) %>%
      ddply(c(id.col), annotateWithSpeeds, ...)
  }
  if(class(x)[1] == "sf"){
    CRS = st_crs(x)
    x$x <- st_coordinates(x)[,1]
    x$y <- st_coordinates(x)[,2]
    df_annotated <- x %>% as.data.frame %>% ddply(c(id.col), annotateWithSpeeds, ...) %>% 
      st_as_sf(crs = CRS)
  }
  return(df_annotated)
}

annotateWithSpeeds <- function(df, time.col = "Time"){
  droplevels(df) %>% mutate(Time = get(time.col)) %>% dplyr::arrange(Time) %>% 
    mutate(z = x+1i*y, 
           dt = c(NA, difftime(Time[-1], Time[-length(Time)], units = "hours") %>% as.numeric), 
           sl = c(NA, Mod(diff(z))), 
           dhours = difftime(Time, Time[1], units = "hours") %>% as.numeric,
           speed = sl/dt)
}  
