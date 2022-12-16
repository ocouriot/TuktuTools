#' get LoCoH
#'
#' This function estimate Local Convex Hulls (LoCoH) by converting a simple feature into a LoCoH-xy
#' object, identifying the nearest neighbors, creating a LoCoH-hullset object by constructing the 
#' local convex polygon using the nearest neighbors (nn) of each data point
#'
#' @param  sf a simple feature containing GPS coordinates used to estimate 
#' the ranging area
#' @param nn the number of nearest neighbors used to construct the local convex polygons
#' @param level A numeric value of the level of the isopleths
#' @param ... additional parameters to pass to \link{kernelUD}
#' 
#' @example examples/example_getKernelUD.R
#' 
#' @return The function returns a simple feature of the isopleth(s), with the area
#' 
#' @export

getLoCoH <- function(sf,  nn = 10, level = .95, 
                     ...){
    xy <- sf %>% st_coordinates()
    
    my.lxy <- tlocoh::xyt.lxy(xy=xy,  
                      id=1,
                      proj4string=CRS(raster::projection(sf)))
    my.lhs <- my.lxy %>% tlocoh::lxy.nn.add(k = nn) %>% 
      tlocoh::lxy.lhs(k = nn) %>% 
      tlocoh::lhs.iso.add(k = nn, iso.levels = c(0,level), iso.add = FALSE, status = FALSE, ...)
    tokeep <- (my.lhs[[1]]$isos[[1]]$polys[2,] %>% st_as_sf(crs=st_crs(sf)))
 return(tokeep)
}
