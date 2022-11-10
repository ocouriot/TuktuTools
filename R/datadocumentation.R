#' Movement data of barren-ground caribou
#'
#' @usage 
#' data(caribou)
#' 
#' @name caribou
#' 
#' @format data frame with the following columns: 
#' #' \describe{
#'   \item{id}{ID of animal}
#'   \item{start}{Date of beginning of collaring}
#'   \item{end}{Date of death or censoring}
#'   \item{fate}{One of "dead", or "censored"}
#' }
#' 
#' name of 4 individuals (ID), the sex, the coordinates 
#' of the GPS locations (Lon,Lat in WGS84 and x,y in Canada Lambert Conformal Conic) 
#' and date and time (Time) of the GPS locations. These are real data courtesy of NWT
#' but anonymized and shifted in time, longitude and latitude. 
#' 
#' @example examples/example_caribou.R
#'
#' @source GNWT Department of Environment and Natural Resources
#' @keywords data
"caribou"
