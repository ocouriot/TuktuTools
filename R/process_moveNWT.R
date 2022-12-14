#' Process Movebank data from NWT
#'
#' This function takes a move object from Movebank from the GNWT data sets and returns it in a format
#' suitably for many of the analyses in this package.
#'
#' @param movedata a Move object, typically obtained with the \code{\link{getMovebankData}} command.
#' @param CRS coordinate reference system, in any format suitable for \code{\link{st_transform}}
#' @param moveLogin a movebankLogin object as: movebankLogin(username="MyUsername", password="MyPassword")
#'
#' @return a dataframe with columns:
#' \describe{
#'   \item{ID}{ID of animal}
#'   \item{nickname}{"nick name" of animal from movebank data}
#'   \item{DateTime}{Date and time in POSIXct}
#'   \item{Year}{year}
#'   \item{Lon,Lat}{Longitude and Latitude (WGS84)}
#'   \item{x,y}{coordinates in the given projection (default: Canada Lambert Conformal Conic)}
#'   \item{sex}{Sex of the individual (f: female, m: male)}
#'   \item{study.site}{Individual's herd assignment}
#' }
#'
#' @export
#' 
#' @examples
#' # to generate the bluenose data
#' \dontrun{
#' bluenose.move <- getMovebankData("GNWT Sahtu Barren Ground Caribou: Bluenose-East",
#' login = mylogin, removeDuplicatedTimestamps=TRUE)
#' bluenose <- process_moveNWT(bluenose.move)}

process_moveNWT <- function(movedata,
                            CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                            mylogin){
  sf <- st_as_sf(movedata) %>% st_transform(CRS)
  df <-  as(movedata, "data.frame")
  df.new <- data.frame(ID = df$trackId,
                       nickname = df$nick_name,
                       Time = df$timestamp,
                       Lon = df$location_long,
                       Lat = df$location_lat,
                       sex = df$sex,
                       Year = lubridate::year(df$timestamp)) %>%
    mutate(x = st_coordinates(sf)[,1], y =  st_coordinates(sf)[,2])
  df.new$ID_temp <- str_replace(df.new$ID,"X","")

  references <- move::getMovebankReferenceTable(slot(movedata,"study"), login = mylogin, allAttributes = FALSE)
  references <- with(references, aggregate(list(study_site = study_site),list(ID_temp=animal_local_identifier),unique))

  df.new <- merge(df.new, references, by="ID_temp", all.x=T) %>% mutate(ID_temp = NULL)
  df.new <- st_as_sf(df.new, coords = c("x","y"), crs = CRS)
  df.new <- df.new %>% mutate(x = st_coordinates(df.new)[,1], y =  st_coordinates(df.new)[,2])
  return(df.new)
}
