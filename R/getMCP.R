#' get MCP
#'
#' wrapper for \link{mcp} in adehabitatHR that returns a given MCP as a simple feature 
#' polygon from a simple feature point collection
#'
#' @param sf a simple feature point collection
#' @param percent MCP, default 95%
#'
#' @return A simple feature polygon of that mcp area
#'
#' @example examples/example_getMCP.R
#' 
#' @export
#' 

getMCP <- function(sf, percent = 95){
  if(class(sf)[1] == "data.frame"){
    sf <- st_as_sf(sf)
  }
  
  ll <- st_coordinates(sf)
  ll.sp <- SpatialPoints(coords = ll)
  ll.mcp <- adehabitatHR::mcp(ll.sp, percent = percent) %>% 
    st_as_sf %>% st_set_crs(st_crs(sf)) %>%
    mutate(id = paste0(percent, "% MCP"))
}


