#' Sociality Index
#'
#' Computes the daily Sociality Index for a set of
#' tracked individuals, defined as
#'
#'   SI = 2 * A * N_enc / (pi * r^2 * n * (n-1))
#'
#' where A is the daily kernel area of collared individuals' daily mean
#' locations, N_enc is the number of unique pairs within distance r, and
#' n is the number of individuals on that day.
#'
#' @param x a data.frame of locations. By default (`dailyMean = TRUE`), `x`
#'   is assumed to ALREADY be daily mean locations (one row per
#'   individual-day, with columns `ID`, `Year`, `yday`, `Lon`, `Lat`) as
#'   produced by `getDailyMean_dt`. Set `dailyMean = FALSE` to pass raw
#'   GPS data and have daily means computed internally.
#' @param r encounter radius in meters (default 200)
#' @param dailyMean logical; if TRUE (default) `x` is treated as daily mean
#'   data and no aggregation is done. If FALSE, `getDailyMean_dt` is called
#'   on `x` first.
#' @param id.col name of individual identifier column
#' @param time.col name of timestamp column (only used when
#'   `dailyMean = FALSE`)
#' @param percent kernel level (default 95)
#' @param min.n minimum number of individuals per day to attempt computation
#'   (default 6: kernel requires > 5 points)
#' @param ... additional arguments passed to `getKernelUD` (e.g. `H`,
#'   `gridsize`).
#'
#' @return data.frame with one row per day: Year, yday, n, N_enc, E_enc,
#'   A_km2, r, SI.
#' @example examples/example_SocialityIndex.R
#' @export

getSocialityIndex <- function(x, r = 200,
                              dailyMean = TRUE,
                              id.col = "ID",
                              time.col = "Time",
                              percent = 95,
                              min.n = 6,
                              parallel = FALSE,
                              ...){
    
    # --- 1. Daily means ---
    if(dailyMean){
        dm <- as.data.frame(x)
    } else {
        cat("Computing daily mean locations ...\n")
        dm <- getDailyMean_dt(x, id.col = id.col, time.col = time.col)
    }
    
    # --- 2. Project to metric CRS centered on data ---
    cat("Projecting to metric CRS ...\n")
    lon0 <- mean(dm$Lon, na.rm = TRUE)
    lat0 <- mean(dm$Lat, na.rm = TRUE)
    proj <- sprintf("+proj=laea +lat_0=%f +lon_0=%f +units=m", lat0, lon0)
    
    dm_sf <- st_as_sf(dm, coords = c("Lon","Lat"), crs = 4326) |>
        st_transform(proj)
    dm_sf$ID <- dm[[ if("ID" %in% names(dm)) "ID" else id.col ]]
    
    # --- 3. Per-day loop: A, N_enc, n, SI ---
    keys <- unique(dm_sf[, c("Year","yday")] |> st_drop_geometry())
    cat("Computing kernels and encounters across", nrow(keys), "days",
        if(parallel) "(parallel) ..." else "...", "\n")
    
    one_day <- function(i){
        yr <- keys$Year[i]; doy <- keys$yday[i]
        d  <- dm_sf[dm_sf$Year == yr & dm_sf$yday == doy, ]
        n  <- nrow(d)
        if(n < min.n) return(NULL)
        
        # Kernel area (km^2)
        ud <- try(getKernelUD(d, percent = percent, ...), silent = TRUE)
        if(inherits(ud, "try-error") || is.null(ud)) return(NULL)
        A_m2 <- as.numeric(sum(ud$area))
        if(!is.finite(A_m2) || A_m2 <= 0) return(NULL)
        
        # Pairwise distances (upper triangle only)
        xy <- st_coordinates(d)
        D  <- as.matrix(dist(xy))
        D[lower.tri(D, diag = TRUE)] <- NA
        N_enc <- sum(D < r, na.rm = TRUE)
        
        # Expected encounters and SI
        E_enc <- pi * r^2 * n * (n - 1) / (2 * A_m2)
        SI    <- N_enc / E_enc
        
        data.frame(Year = yr, yday = doy, n = n,
                   N_enc = N_enc, E_enc = E_enc,
                   A_km2 = A_m2 / 1e6, r = r, SI = SI)
    }
    
    if(parallel){
        res <- future.apply::future_lapply(seq_len(nrow(keys)), one_day,
                                           future.seed = TRUE)
        out <- plyr::ldply(res)
    } else {
        out <- plyr::ldply(seq_len(nrow(keys)), one_day, .progress = "text")
    }
    
    cat("Done.\n")
    out <- out[order(out$Year, out$yday), ]
    rownames(out) <- NULL
    out
}
