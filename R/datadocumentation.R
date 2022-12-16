#' Movement data of barren-ground caribou
#'
#' @usage 
#' data(caribou)
#' 
#' @name caribou
#' 
#' @format data frame with the following columns: 
#' #' \describe{
#'   \item{ID}{ID of animal}
#'   \item{sex}{Sex of the animal}
#'   \item{Time}{Date and time of each GPS location}
#'   \item{Year}{Year of the GPS location}
#'   \item{Lon,Lat}{Coordinates of the GOS locations in WGS84}
#'   \item{x,y}{Coordinates of the GOS locations in Canada Lambert Conformal Conic (crs = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")}
#' }
#' 
#' These are real data courtesy of NWT Department of Environment and Natural Resources
#' but anonymized and shifted in time, longitude and latitude. 
#' 
#' @example examples/example_caribou.R
#'
#' @source GNWT Department of Environment and Natural Resources
#' @keywords data
"caribou"
