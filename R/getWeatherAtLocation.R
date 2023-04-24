#' Get Weather at Location
#' 
#' Given a known geographical location, this function downloads the corresponding 
#' raw text file of observations for the closest station from 
#' "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/gsn/", and converts the relevant 
#' average, maximum, minimum daily temperature into a dataframe.
#' This function also reports the distance between the given geographical location and 
#' the corresponding meteostation. 
#' 
#' @param {longitude,latitude} longitude and latitude of the known geographical location
#' @param start first year of observation.  The function will keep widening its 
#' search radius until it finds a station with data reaching back to the startyear.  
#' Also, the returned data will only reach back to the start year. 
#' @param end last year of observations. 
#' @param threshold distance within which to limit search for weather stations (default 200 km)
#' @param dir directory to store the raw observations 
#' @param keep.raw whether or not to keep the raw file
#' 
#' @example examples/example_getWeatherAtLocation.R
#' 
#' @export
#' 

getWeatherAtLocation <- function(longitude, latitude, start, end, 
                                 threshold = 200, 
                                 dir = tempdir(check = TRUE),
                                 keep.raw = FALSE){
  data("gsn_stations")
  data("gsn_stations_withyear")
  
  getDistance <- function(long1, lat1, long2, lat2) {
    R <- 6371 # Earth mean radius [km]
    d <- acos(sin(lat1*pi/180)*sin(lat2*pi/180) + 
                cos(lat1*pi/180)*cos(lat2*pi/180) * 
                cos(long2*pi/180-long1*pi/180)) * R
    return(d) # Distance in km
  }
  getDistances <- Vectorize(getDistance, c("long2", "lat2"))
  distances <- getDistances(longitude, latitude, gsn_stations$lon, gsn_stations$lat)
  
  gsn_close <- subset(gsn_stations, distances < threshold)
  stations_sorted  <-  gsn_close$station[order(distances[distances < threshold])]
  distances_sorted <- sort(distances[distances < threshold])
  
  plant.year <- c(start:end)
  dailytemp_1 <- data.frame()
  for(i in 1:length(stations_sorted)){
    mystation <- stations_sorted[i]
    print(paste(i, mystation))
    
    mydistance <- distances_sorted[i]
    mydailytemp <- downloadWeather(mystation, dir = dir)
    
    if(nrow(mydailytemp) > 1 & any(mydailytemp$Year %in% plant.year)){
      contain <- mydailytemp[mydailytemp$Year %in% plant.year,]
      dailytemp_1 <- gtools::smartbind(dailytemp_1, contain)
      ds <- min(dailytemp_1$Year) 
      de <- max(dailytemp_1$Year) 
      cat(paste0("The ", i, "th closest station is ", mystation, 
                 "\nThe distance is ", round(mydistance, 3), 
                 " km.\nData to be covered from ", ds, " to ", de, ".\n"))
      
      plant.year <- plant.year[!(plant.year %in% dailytemp_1$Year)]
      
      tempfiles <- list.files(dir, pattern = "raw")
      if(!keep.raw) for(f in tempfiles) try(file.remove(paste0(dir,"\\",f)))
      dailytemp_1[,c("Tmin","Tmax","Tavg", "prcp")] <- 
        apply(dailytemp_1[,c("Tmin","Tmax","Tavg", "prcp")], 2, 
              function(x){ x[x < -999] <- NA; x})
    }
  }    
  return(dailytemp_1)
}
