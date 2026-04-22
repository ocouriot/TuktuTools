#'get Kernel UD
#'
#' wrapper for \link{kde} functino in ks that returns a given UD
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
    if(class(sf)[1] == "data.frame") sf <- st_as_sf(sf)
    ll <- st_coordinates(sf)
    fit <- ks::kde(ll, gridsize = c(grid, grid), ...)
    lev <- ks::contourLevels(fit, prob = 1 - percent/100)
    
    cl <- contourLines(fit$eval.points[[1]], fit$eval.points[[2]],
                       fit$estimate, levels = lev)
    cl <- cl[sapply(cl, function(c) length(c$x) >= 4)]
    if (length(cl) == 0) return(NULL)
    
    polys <- lapply(cl, function(c) {
        xy <- cbind(c$x, c$y)
        if (!identical(xy[1,], xy[nrow(xy),])) xy <- rbind(xy, xy[1,])
        xy <- xy[!duplicated(xy),]
        xy <- rbind(xy, xy[1,])
        st_polygon(list(xy))
    })
    
    ll.poly <- withr::with_options(
        list(sf_use_s2 = FALSE),
        st_sf(geometry = st_sfc(polys, crs = st_crs(sf))) |> st_make_valid()
    )
    ll.kernel <- st_sf(geometry = st_union(ll.poly))
    ll.kernel$area <- st_area(ll.kernel)

    return(ll.kernel)
}
