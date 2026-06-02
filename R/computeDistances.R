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
computeDistances <- function(df,
                             X.col     = "X",
                             Y.col     = "Y",
                             time.col  = "Time",
                             start     = "02-01",
                             end       = "06-01",
                             rate      = 8,
                             tolerance = rate / 2) {
    
    ## 1. Date-window filter -----------------------------------------------
    md <- format(df[[time.col]], "%m-%d")
    df <- df[md >= start & md <= end, , drop = FALSE]
    df <- df[order(df[[time.col]]), ]
    
    if (nrow(df) < 2) {
        message("computeDistances: fewer than 2 fixes after date filtering; returning NULL.")
        return(NULL)
    }
    
    ## 2. Resample ---------------------------------------------------------
    lag_hr    <- c(NA_real_, as.numeric(diff(df[[time.col]]), units = "hours"))
    in_window <- !is.na(lag_hr) &
        lag_hr >= (rate - tolerance) &
        lag_hr <= (rate + tolerance)
    in_window[1] <- TRUE
    df <- df[in_window, ]
    
    if (nrow(df) < 2) {
        message("computeDistances: fewer than 2 fixes after resampling; returning NULL.")
        return(NULL)
    }
    
    ## 3. Distances via complex arithmetic ---------------------------------
    z <- df[[X.col]] + 1i * df[[Y.col]]
    
    data.frame(
        n_fixes          = nrow(df),
        total_distance   = sum(Mod(diff(z)), na.rm = TRUE) / 1000,
        net_displacement = Mod(z[length(z)] - z[1])        / 1000
    )
}
