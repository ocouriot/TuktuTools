#' getBasemapRaster
#' 
#' This function obtains an RGB raster from any of the available base maps in the (incredible!) 
#' open source leaflet package.  It is meant to replace - in a way - the \code{\link{ggmap}} 
#' function, which now requires an API key with Google.  It is in some ways more flexible: you 
#' can set the lat and long limits strictly, it returns a projected raster which can be reprojected 
#' to any other resolution, and it can access any of the remarkable diversity of high quality 
#' mapping option at:  http://leaflet-extras.github.io/leaflet-providers/preview/.  Note that it 
#' creates a "png" and "html" and places it in a given directory, and does not delete them. 
#'  
#' @param x Either the xmin limit of longitude, or a bbox from a spatial object.
#' @param {xmax,ymin,ymax} Limits (in longitude and latitude) of desired map raster. Only used if
#' `x` is not a bbox.
#' @param map.types Character specification for the base maps. see http://leaflet-extras.github.io/leaflet-providers/preview/ for available options. Favorites include: \code{Esri.WorldPhysical} (default), \code{Esri.WorldTerrain}, \code{Esri.NatGeoWorldMap}
#' @param filename name of png and html files
#' @param directory directory to save the html and png files
#' @param {width,height} approximate width and height of final raster; this is VERY approximate 
#' because the final raster is cropped around the desired limits and because there is some 
#' @param zoom this is the zoom argument from \code{\link{webshot}}; it also has some complicated 
#' effect on the resolution of the final image.
#' @param plotme whether or not to plot the raster with \code{\link{plotRGB}}. Note that high 
#' resolution rasters are reduced in rendering within R by default ... this can be modified 
#' with \code{\link{plotRGB}} options.
#' @param output_crs output coordinate reference system, defiend as a proj4string text.
#' 
#' @return An RGB raster, i.e. one with three levels for each of the colors. Note, the projection of the returned raster 
#' is the Spherical Mercator (EPSG:3857) - used for global tiling and "native" to mapview (and leaflet). 
#' 
#' @examples
#' # SE Alaska
#' bb <- st_bbox(c(xmin = -138, xmax = -130, ymax = 56, ymin = 60), crs = st_crs(4326))
#' SEalaska.topo <- getBasemapRaster2(bb, "OpenTopoMap", plotme = TRUE)
#' # with reprojection
#' SEalaska.topo <- getBasemapRaster2(bb, "OpenTopoMap", output_crs = "+init=epsg:4326", 
#' plotme = TRUE)
#' # for a ggPlot use this function (from RStoolbox): 
#' library(RStoolbox)
#' ggRGB(SEalaska.topo, 1, 2, 3, coord_equal = FALSE)
#' # using GPS data
#' NWT_sat <- boreal_ss %>%
#'   get_bbox(crs = 4326) %>% 
#'   getBasemapRaster2("Esri.WorldImagery", plotme = T)
#' 
#' @export

getBasemapRaster <- function(x,
                              xmax = NULL, ymin = NULL, ymax = NULL, 
                              map.types = "Esri.WorldPhysical",
                              directory = ".", 
                              filename = "basemap",
                              width = 1000, height = 1000, zoom = 1, 
                              plotme = FALSE, 
                              output_crs = "+init=epsg:3857", ...)
{
  if(!is.na(st_bbox(x))) {
    # we could still take the bbox of a spatial object, if we want.
    x <- unname(x)
    xmin <- x[1]
    xmax <- x[3]
    ymin <- x[2]
    ymax <- x[4]
  } else {
    xmin <- x
  }
  
  # creating a white rectangular donut
  outer = cbind(lon = c(-170,-170,170,170,-170), lat = c(-89,89,89,-89,-89))
  hole = cbind(lon = c(xmin, xmax, xmax, xmin, xmin), 
               lat = c(ymin, ymin, ymax, ymax, ymin))

  earthwithhole = st_polygon(list(outer, hole)) %>% st_sfc(crs = 4326) %>% st_transform(3857)
  
  basemap <- mapview::mapview(earthwithhole, 
                              map.types = map.types, alpha = 0.5,
                     col.regions = "white", alpha.regions = 1) 
  basemap@map <- leaflet::fitBounds(basemap@map, xmin, ymin, xmax, ymax)
  
  mapurl <- paste0(directory, '/', filename, '.html')
  mappng <- paste0(directory, '/', filename, '.png')
  
  # create local html  
  mapview::mapshot(basemap, url = mapurl)
  # create png from local html
  webshot::webshot(url = mapurl, file = mappng, vwidth = width*2, vheight = height*2, zoom = zoom)
  #zoom = sqrt(width/1000))
  m <- satellite::brick(paste0(directory, '/', filename, '.png'))
  
  # trim off legend and other mapview text at bottom 
  m <- satellite::crop(m, extent(0.1 * dim(m)[1],
                      0.9 * dim(m)[1],
                      0.1 * dim(m)[2],
                      0.9 * dim(m)[2]))
  
  innercore <- m[[1]] != 255
  innercore[!innercore] <- NA
  trimextent <- trim(innercore) %>% extent
  
  m2 <- satellite::crop(m, trimextent)
  
  # convert the desired bounding box to 3857
  xy.new <- c(st_point(c(xmin, ymin)), st_point(c(xmax, ymax))) %>% st_sfc(crs = 4326) %>% 
    st_transform(3857) %>% st_coordinates
  
  extent(m2) <- extent(t(xy.new[,1:2]))
  crs(m2) <- CRS("+init=epsg:3857")
  
  if(output_crs != "+init=epsg:3857") m2 <- raster::projectRaster(m2, crs = CRS(output_crs))
  
  if(plotme) raster::plotRGB(m2)
  return(m2)
}




