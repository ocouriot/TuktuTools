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


#' Simulated migrations of 18 caribou
#'
#' Simulated migration data provided to illustrate some of the features of the 
#' TuktuTools package.  Two objects are loaded: 
#' \describe{
#' \item{\code{simulated_migrations} is a data frame with Lat-Long coordinates,}
#' \item{\code{simulated_migrations.sf} is a georeferenced simple feature object.} 
#'}
#'
#' @details These are simulated data of 18 caribou over the period of a 
#' spring migration in 2014, with data from March 31 through June 24.  The 
#' simulations were generated using statistical summaries of spring migration 
#' of Bathurst caribou in 2014 reported by Gurarie et al. (2019).  The tracks 
#' were generated via some modifications of the \code{\link[marcher]{simulate_shift}} 
#' function in the \code{marcher} packages. 
#' 
#' @usage 
#' data(simulated_migrations)
#' 
#' @name simulated_migrations
#' 
#' @format data frame with the following columns: 
#' #' \describe{
#'   \item{ID}{Numberic ID of animal}
#'   \item{Name}{Name of animal}
#'   \item{Time}{Date and time of each GPS location (POSIX)}
#'   \item{Lon,Lat}{Coordinates locations in WGS84}
#'   \item{geometry}{simple feature geometry}}
#' 
#' @example examples/example_simulated_migrations.R
#'
#' @source Gurarie et al. (2019) Tactical departures and strategic arrivals: Divergent effects of climate and weather on caribou spring migrations. Ecosphere 10.12 (2019): e02971.
#' @source Gurarie & Cheraghi (2017). marcher: Migration and Range Change Estimation in R. R package version 0.0-2,
#' @keywords data
"simulated_migrations"
