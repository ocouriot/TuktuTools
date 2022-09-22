#' get base map raster

getMap <- function (gg.ext, map_service, map_type, map_token, map_dir, 
          map_res, m.crs) 
{
  if (inherits(gg.ext, "bbox")) 
    gg.ext <- list(gg.ext)
  r <- lapply(gg.ext, function(y) {
    gg.ext.ll <- st_bbox(st_transform(st_as_sfc(y), crs = st_crs("+init=epsg:4326")))
    tg <- bbox_to_tile_grid(gg.ext.ll, max_tiles = ceiling(map_res * 
                                                             20))
    images <- apply(tg$tiles, MARGIN = 1, function(x) {
      file <- paste0(map_dir, map_service, "_", map_type, 
                     "_", x[1], "_", x[2], ".png")
      retry <- list(do = TRUE, count = 0)
      while (retry$do) {
        url <- paste0(getOption("moveVis.map_api")[[map_service]][[map_type]], 
                      tg$zoom, "/", x[1], "/", x[2], 
                      ".png", if (map_service == "mapbox") 
                        paste0("?access_token=", map_token)
                      else NULL)
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
    #crop(projectRaster(r, crs = m.crs), extent(y[1], y[3], 
    #                                           y[2], y[4]), snap = "out")
  })
  if (length(r) > 1) {
    ext.both <- list(east = extent(r$east), west = extent(r$west))
    rg <- c(east = diff(c(ext.both$east@xmin, ext.both$east@xmax)), 
            west = diff(c(ext.both$west@xmin, ext.both$west@xmax)))
    ext.both <- .expand_ext(ext.both, rg)
    extent(r$east) <- ext.both$east
    extent(r$west) <- ext.both$west
    ext.combi <- .combine_ext(ext.both)
    r[[which.min(rg)]] <- extend(r[[which.min(rg)]], ext.combi)
    r[[which.max(rg)]] <- resample(r[[which.max(rg)]], r[[which.min(rg)]])
    r <- list(merge(r[[1]], r[[2]]))
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
