#' get LoCoH and get Daily Range
#'
#' This function estimate (daily) Local Convex Hulls (LoCoH) by converting a simple feature into a LoCoH-xy
#' object, identifying the nearest neighbors, creating a LoCoH-hullset object by constructing the 
#' local convex polygon using the nearest neighbors (nn) of each data point
#'
#' @param  sf a simple feature containing (daily) GPS coordinates used to estimate 
#' the (daily) ranging area
#'@param nn the number of nearest neighbors used to construct the local convex polygons
#'@param level A numeric value of the level of the isopleths
#'@param CRS the CRS of the simple feature (default is "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#' @return The function returns a simple feature of the isopleth(s), with the area

getLoCoH <- function(sf,  nn = 10, level = .90, 
                     CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                     ...){
    sf <- sf %>% st_as_sf(crs=CRS)
    xy <- sf %>% st_coordinates()
    my.lxy <- xyt.lxy(xy=xy,  
                      id=1,
                      proj4string=CRS(projection(sf)))
    my.lhs <- my.lxy %>% lxy.nn.add(k = nn) %>% 
      lxy.lhs(k = nn) %>% 
      lhs.iso.add(k = nn, iso.levels = c(0,level), iso.add = FALSE, status = FALSE)
    tokeep <- (my.lhs[[1]]$isos[[1]]$polys %>% st_as_sf(crs = CRS))[2,]
 return(tokeep)
}

getDailyRange <- function(sf){
  daily.area <- list()
  for(doy in sort(unique(df$yday))){
    doy.loc <- subset(df, yday == doy)
    if(dim(doy.loc)[1] > 5){
      ddayarea <- try(getLoCoH(doy.loc), silent=TRUE)
      if(!inherits(ddayarea, 'try-error')){
        ddayarea <- ddayarea %>% mutate(Year = unique(df$Year), yday = doy)
        daily.area[[length(daily.area)+1]] <- ddayarea
      }
    }
  }
  daily.area <- daily.area %>%ldply
  return(daily.area)
}