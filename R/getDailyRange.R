#' get Daily Range
#'
#' This function estimate (daily) Local Convex Hulls (LoCoH) by converting a simple feature into a LoCoH-xy
#' object, identifying the nearest neighbors, creating a LoCoH-hullset object by constructing the 
#' local convex polygon using the nearest neighbors (nn) of each data point
#'
#' @param  sf a simple feature containing (daily) GPS coordinates used to estimate 
#' the (daily) ranging area
#' @param method either "LocoH" or "KernelUD", the method used to estimate the daily ranging area (see getLoCoH and getKernelUD for more details)
#' @param nn (if method is "LoCoH") the number of nearest neighbors used to construct the local convex polygons
#' @param level (if method is "LoCoH") A numeric value of the level of the isopleths (value comprised between .05 and 1, default is .95)
#' @param percent (if method is "KernelUD") percent kernel (value comprised between 5 and 100, default is 95)
#' @param grid (if method is "KernelUD") grid points (default 200, higher than adehabitat's)
#' @param ... additional parameters to pass to \link{kernelUD} for the "KernelUD" method or \link{lhs.iso.add} for the "LoCoH" method
#' 
#' @return The function returns a simple feature of the isopleth(s), with the area
#' 
#' @export
#' 
#' @example examples/example_getDailyRange.R

getDailyRange <- function(sf, crs, method = c("LoCoH", "KernelUD"), ...){
  sf <- sf %>% st_as_sf(crs=crs)
  daily.area <- list()
  for(doy in sort(unique(sf$yday))){
    doy.loc <- subset(sf, yday == doy)
    if(dim(doy.loc)[1] > 5){
      if(method == "LoCoH"){
        ddayarea <- try(getLoCoH(doy.loc, ...), silent=TRUE)
        if(!inherits(ddayarea, 'try-error')){
          ddayarea <- ddayarea %>% mutate(Year = unique(sf$Year), yday = doy)
          daily.area[[length(daily.area)+1]] <- ddayarea
        }
      }
      if(method == "KernelUD"){
        ddayarea <- try(getKernelUD(doy.loc, ...), silent=TRUE)
        if(!inherits(ddayarea, 'try-error')){
          ddayarea <- ddayarea %>% mutate(Year = unique(sf$Year), yday = doy)
          daily.area[[length(daily.area)+1]] <- ddayarea
        }
      }
    }
  }
  daily.area <- daily.area %>% ldply(st_as_sf, crs=st_crs(sf))
  return(daily.area %>% st_as_sf(crs = st_crs(sf)))
}