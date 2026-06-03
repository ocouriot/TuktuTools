#' Compute migration distances for a single individual-year
#'
#' Resamples a telemetry track to a regular time interval and computes total
#' path length (total distance) and straight-line displacement (net distance)
#' over a specified date window. Designed to be called within a
#' \code{group_by / summarise} or \code{nest / map} workflow across IDs and years.
#'
#' @param df A data frame with at minimum columns for longitude, latitude, and
#'   time. Column names controlled by \code{X.col}, \code{Y.col}, and
#'   \code{time.col}.
#' @param X.col Character. Name of the longitude column. Default \code{"X"}.
#' @param Y.col Character. Name of the latitude column. Default \code{"Y"}.
#' @param time.col Character. Name of the POSIXct datetime column.
#'   Default \code{"Time"}.
#' @param start Character \code{"MM-DD"} giving the start of the date window
#'   (inclusive). Default \code{"02-01"}.
#' @param end Character \code{"MM-DD"} giving the end of the date window
#'   (inclusive). Default \code{"06-01"}.
#' @param rate Numeric. Resampling interval in hours. Default \code{8}.
#' @param tolerance Numeric. Tolerance around \code{rate} (in hours) passed to
#'   \code{amt::track_resample}. Default \code{rate / 2}.
#' @param cushion_days number of days within the start and end within which to 
#'  allow the calculation
#' @param ... Additional arguments passed to \code{amt::track_resample}.
#'
#' @return Data frame with:
#'   \describe{
#'     \item{n_fixes}{Number of resampled fixes used.}
#'     \item{total_distance}{Total path length in kilometres.}
#'     \item{net_displacement}{Straight-line distance from first to last fix, in kilometres.}
#'   }
#'
#' @details
#' Workflow:
#' \enumerate{
#'   \item Filter rows to the \code{start}–\code{end} date window.
#'   \item Build an \pkg{amt} track and reproject to \code{crs_to}.
#'   \item Resample to \code{rate} hours (± \code{tolerance}).
#'   \item Compute distances using complex-number arithmetic on the projected
#'         coordinates (\code{z = x_ + 1i * y_}).
#' }
#'
#' @example examples/example_computeDistances.R
#' @export
#' 
computeDistances <- function(df,
                             X.col  = "X", Y.col = "Y", time.col  = "Time",
                             start  = "02-01", end  = "06-01",
                             rate  = 8, tolerance = 2, 
                             cushion_days = 7) {
    
    ## 1. Date-window filter -----------------------------------------------
    md <- format(df[[time.col]], "%m-%d")
    df <- df[md >= start & md <= end, , drop = FALSE]
    df <- df[order(df[[time.col]]), ]
    
    if (nrow(df) < 2) {
        message("computeDistances: fewer than 2 fixes after date filtering; returning NULL.")
        return(NULL)
    }
    
    start_doy <- yday(ymd(paste0("2001-", start)))
    end_doy   <- yday(ymd(paste0("2001-", end)))
    
    if (yday(min(df[[time.col]])) > (start_doy + cushion_days) |
        yday(max(df[[time.col]])) < (end_doy   - cushion_days)) {
        message(sprintf("computeDistances: track does not span full window (got %s to %s); returning NULL.",
                        format(min(df[[time.col]]), "%m-%d"),
                        format(max(df[[time.col]]), "%m-%d")))
        return(NULL)
    }
    
    ## 2. Phase-optimised resampling ---------------------------------------
    t_min <- min(df[[time.col]])
    t_max <- max(df[[time.col]])
    
    ## Try each possible phase shift (one per fix in the first rate-window)
    ## so that the regular grid is anchored to an observed fix
    phase_starts <- df[[time.col]][df[[time.col]] <= t_min + 3600 * rate]
    
    best <- NULL
    for (t0 in phase_starts) {
        class(t0) <- class(t_min)   # preserve POSIXct after loop indexing
        time_target <- seq(t0, t_max, by = 3600 * rate)
        
        ## For each target, find nearest observed fix
        nearest_idx <- sapply(time_target, function(t) which.min(abs(df[[time.col]] - t)))
        nearest_t   <- df[[time.col]][nearest_idx]
        match_hr    <- abs(as.numeric(difftime(time_target, nearest_t, units = "hours")))
        
        keep    <- which(match_hr < tolerance)
        n_gaps  <- length(time_target) - length(keep)
        
        if (is.null(best) || n_gaps < best$n_gaps) {
            best <- list(idx    = nearest_idx[keep],
                         n_gaps = n_gaps,
                         n_expected = length(time_target))
        }
    }
    
    df_filtered <- df[best$idx, ]
    
    if (nrow(df) < 2) {
        message("computeDistances: fewer than 2 fixes after resampling; returning NULL.")
        return(NULL)
    }
    
    ## 3. Distances via complex arithmetic ---------------------------------
    z_filtered <- df_filtered[[X.col]] + 1i * df_filtered[[Y.col]]
    z_raw <- df[[X.col]] + 1i * df[[Y.col]]
    
    data.frame(
        n_fixes        = nrow(df),
        n_gaps         = best$n_gaps,
        n_expected     = best$n_expected,
        total_distance   = sum(Mod(diff(z_filtered)), na.rm = TRUE) / 1000,
        net_displacement = Mod(z_raw[length(z_raw)] - z_raw[1]) / 1000
    )
}
