#'get Kernel UD
#'
#' wrapper for \link{kernelUD} in adehabitatLT that returns a given UD
#' as a simple feature polygon from a simple feature point collection
#'
#' @param sf a simple feature point collection
#' @param percent percent kernel
#' @param grid grid points (default 200, higher than adehabitat's)
#' @param ... additional parameters to pass to \link{kernelUD}.  Note, 
#' in particular, that \code{method = "epa"}
#' @returns A simple feature polygon of the kernel area
#' @example examples/example_getKernelUD.R
#' 
#' @export

getKernelUD <- function(sf, percent = 95, grid = 200, ...){
  ll <- st_coordinates(sf)
  ll.sp <- SpatialPoints(coords = ll)
  ll.kernel <- adehabitatHR::kernelUD(ll.sp, grid = grid, ...) %>% 
    adehabitatHR::getverticeshr(percent)
  ll.poly <- st_as_sf(ll.kernel) %>% st_cast("POLYGON")
  ll.poly$area <- st_area(ll.poly)
  ll.poly[which.max(ll.poly$area),] %>% st_set_crs(st_crs(sf))
}
