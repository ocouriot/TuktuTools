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

get.speed <- function(df,
                      CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"){
  df <- as.data.frame(df) %>% mutate(ID_Year = paste(ID, year(Time), sep = "_"))
  #table to be returned
  speed.df <- data.frame()
  for (i in unique(df$ID_Year)) {

    tempo <- droplevels(subset(as.data.frame(df), ID_Year == i))
    tempo <- tempo[order(tempo$Time),]
    xn <- tempo$x
    yn <- tempo$y
    timen <- tempo$Time
    # calculate the step lengths between subsequent relocations
    sl <- Mod(diff(xn + 1i*yn))
    # calculate the timelags between subsequent relocations
    dt <- difftime(timen[-1], timen[-length(timen)], units = "hours")
    # calculate the speed between subsequent relocations
    speed <- sl/dt %>% as.numeric
    # calculate the date and time exactly in the middle between subsequent relocations
    time.mid <- as.POSIXct(c(timen[-length(timen)] + dt/2,NA),origin="1970-01-01")

    # time difference (in hours) between each step length and the first one
    dhours <- difftime(time.mid, time.mid[1], units = "hours") %>% as.numeric

    # table to be returned
    tempo$speed <- c(speed,NA)
    tempo$time.mid <- time.mid
    tempo$dhours <- dhours
    tempo$dt <- c(as.numeric(dt),NA)
    speed.df <- rbind(speed.df, tempo)
    attr(speed.df, "projection") <- CRS
  }

  return(speed.df %>% mutate(ID_Year = NULL))
}




