#' get base map raster
#' 
#' Based on the (hidden) getMap function in moveVis, with some slight modifications for 
#' steamlining. 
#' 
#' @param map_service workd best with the default "osm" (Open street map) 
#' @param map_type see options in \code{\link{moveVis::get_maptypes()}}
#' @param m.crs Coordinate reference system
#' @param map_dir a location to (temporarily) store basemap tiles
#' @param delete_tiles whether or not to clear the basemap directory. 
#' 
#' @example examples/example_getBasemap.R
#' 

getBasemap <- function (gg.ext, map_service = "osm", map_type = "terrain_bg", map_res, 
                        m.crs = NULL, map_dir = "basemap", delete_tiles = TRUE) 
{
  #if(dir.exists(map_dir)){
  #  warning("Please create a folder (by default: map_dir = 'basemap') to store downloaded files.")
  #  stop()
  #}
  
    gg.ext.ll <- st_bbox(st_transform(st_as_sfc(gg.ext), 
                                      crs = st_crs("+init=epsg:4326")))
    
    tg <- bbox_to_tile_grid(gg.ext.ll, max_tiles = ceiling(map_res * 
                                                             20))
    images <- apply(tg$tiles, MARGIN = 1, function(x) {
      file <- paste0(map_dir, "/", map_service, "_", map_type, 
                     "_", x[1], "_", x[2], ".png")
      retry <- list(do = TRUE, count = 0)
      while (retry$do) {
        url <- paste0(getOption("moveVis.map_api")[[map_service]][[map_type]], 
                      tg$zoom, "/", x[1], "/", x[2], 
                      ".png")
        if (!file.exists(file)) 
          curl_download(url = url, destfile = file)
        catch <- try(image_read(file), silent = T)
        if (inherits(catch, "try-error")) {
          unlink(file)
          retry$count <- retry$count + 1
          if (retry$count < 10) 
            retry$do <- TRUE
          else out(paste0("Base map download failed: ", 
                          catch), type = 3)
        }
        else {
          retry$do <- FALSE
        }
      }
      image_write(image_convert(image_read(file), format = "PNG24"), 
                  file)
      return(file)
    })
    r <- quiet(compose_tile_grid(tg, images))
    if(!is.null(m.crs)){
      r <- projectRaster(r, crs = m.crs) %>%
      crop(c(gg.ext[[1]],gg.ext[[3]],gg.ext[[2]],gg.ext[[4]]))
    }
  # delete tiles 
  if(delete_tiles){
    files <- paste0(map_dir,"/",list.files(map_dir))
    files <- files[grepl(paste0(map_service, "_", map_type), files)]
    lapply(files, file.remove)
  }
  return(r)
}


bbox_to_tile_grid <- function (bbox, zoom = NULL, max_tiles = NULL) 
{
  if (purrr::is_null(zoom) && purrr::is_null(max_tiles)) {
    stop("at least one of the zoom or max_tiles arugments must be supplied")
  }
  if (purrr::is_null(zoom)) {
    tile_query <- bbox_tile_query(bbox, zoom_levels = 0:19)
    suitable_zooms <- tile_query$total_tiles <= max_tiles
    zoom <- tile_query$zoom[max(which(suitable_zooms))]
  }
  tile_extent <- bbox_tile_extent(bbox, zoom)
  x_tiles <- tile_extent$x_min:tile_extent$x_max
  y_tiles <- tile_extent$y_min:tile_extent$y_max
  if (!purrr::is_null(max_tiles) && (length(x_tiles) * length(y_tiles)) > 
      max_tiles) {
    stop("Bounding box needed more than max_tiles at specified zoom level. Check with bbox_tile_query(bbox)")
  }
  tile_grid <- list(tiles = expand.grid(x = x_tiles, y = y_tiles), 
                    zoom = zoom)
  class(tile_grid) <- "tile_grid"
  tile_grid
}


out <- function (input, type = 1, ll = NULL, msg = FALSE, sign = "", 
          verbose = getOption("moveVis.verbose")) 
{
  if (is.null(ll)) 
    if (isTRUE(verbose)) 
      ll <- 1
    else ll <- 2
    if (type == 2 & ll <= 2) {
      warning(paste0(sign, input), call. = FALSE, immediate. = TRUE)
    }
    else {
      if (type == 3) {
        stop(input, call. = FALSE)
      }
      else {
        if (ll == 1) {
          if (msg == FALSE) {
            cat(paste0(sign, input), sep = "\n")
          }
          else {
            message(paste0(sign, input))
          }
        }
      }
    }
}


quiet <- function (expr) 
{
  return(suppressWarnings(suppressMessages(expr)))
}
