#'get Kernel UD
#'
#' wrapper for \link{kernelUD} in adehabitatLT that returns a given UD
#' as a simple feature polygon from a simple feature point collection
#'
#' @param my.sf a simple feature point collection
#' @param percent percent kernel
#' @param grid grid points (default 200, higher than adehabitat's)
#' @param ... additional parameters to pass to \link{kernelUD}
#' @example examples/example_getKernelUD.R

getKernelUD <- function(my.sf, percent = 95, grid = 200, ...){
  ll <- st_coordinates(my.sf)
  ll.sp <- SpatialPoints(coords = ll)
  ll.kernel <- kernelUD(ll.sp, grid = grid, ...) %>% getverticeshr(percent)
  ll.poly <- st_as_sf(ll.kernel) %>% st_cast("POLYGON")
  ll.poly$area <- st_area(ll.poly)
  ll.poly[which.max(ll.poly$area),] %>% st_set_crs(st_crs(my.sf))
}
