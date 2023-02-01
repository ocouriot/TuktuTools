#' Download Dailytemp
#' 
#' Given a known station ID, this function automatically downloads the corresponding daily temperature file 
#' from "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/gsn/" and converts the relevant average, maximum, 
#' minimum daily temperature into a dataframe. 
#' 
#' @references The journal article describing GHCN-Daily is: 
#' Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012:  An overview 
#' of the Global Historical Climatology Network-Daily Database.  Journal of Atmospheric 
#' and Oceanic Technology, 29, 897-910, doi:10.1175/JTECH-D-11-00103.1.
#' 
#' To acknowledge the specific version of the dataset used, please cite:
#' Menne, M.J., I. Durre, B. Korzeniewski, S. McNeal, K. Thomas, X. Yin, S. Anthony, R. Ray, 
#' R.S. Vose, B.E.Gleason, and T.G. Houston, 2012: Global Historical Climatology Network - 
#'   Daily (GHCN-Daily), Version 3. [indicate subset used following decimal, 
#'                                     e.g. Version 3.12]. 
#' NOAA National Climatic Data Center. http://doi.org/10.7289/V5D21VHZ
#' 
#' @param {station} station ID
#' @param {dir} folder to save the file in (default: temporary directory)
#' 
#' @examples 
#' mydata <- downloadDailytemp("AO000066160")
#' @export
#' 


downloadDailytemp <- function(station, dir = tempdir(check = TRUE)){
  data("gsn_stations")
  
  url <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
  
  download.file(paste0(url,station,".dly"), destfile = paste0(dir, "/raw_",station,".txt"))
  
  file.name <- paste0(dir, "/raw_",station, ".txt")
  keepcols <- c(2:4, seq(5,65,2))
  mywidths <- c(11,4,2,4,rep(c(5,3), 31))
  
  mytemp <- read.fwf(file = file.name, widths = mywidths, 
                     header = FALSE, sep="\t", na.strings='-999.9')[keepcols]
  
  mytemp_sort <-  arrange(mytemp, V2,V3,V4) %>% mutate(station = station)
  
  testing <- mytemp_sort
  prcp <- subset(testing, V4 == "PRCP")
  min <- subset(testing, V4 == "TMIN") 
  max <- subset(testing, V4 == "TMAX")
  avg <- subset(testing,V4=="TAVG")
  
  # steps below could be cleaned up with a processWeather function
  if(nrow(prcp)>0){
    ym.prcp <- unique(prcp[,c("V2", "V3")])
    process.prcp <- data.frame(Station = mytemp_sort$station[1],
                              Year = rep(ym.prcp[,1], each = 31),
                              Month = rep(ym.prcp[,2], each = 31),
                              Day = 1:31,
                              prcp = (prcp[,4:34] %>% as.matrix %>% t %>% as.vector) / 10
    ) }
  
  if(nrow(min)>0){
    ym.min <- unique(min[,c("V2", "V3")])
    process.min <- data.frame(Station = mytemp_sort$station[1],
                              Year = rep(ym.min[,1], each = 31),
                              Month = rep(ym.min[,2], each = 31),
                              Day = 1:31,
                              Tmin = (min[,4:34] %>% as.matrix %>% t %>% as.vector) / 10
    ) }
  
  if(nrow(avg)>0){    
    ym.avg <- unique(avg[,c("V2", "V3")])
    process.avg <- data.frame(Station = mytemp_sort$station[1],
                              Year = rep(ym.avg[,1], each = 31),
                              Month = rep(ym.avg[,2], each = 31),
                              Day = 1:31,
                              Tavg = (avg[,4:34] %>% as.matrix %>% t %>% as.vector) / 10
    )}
  
  if(nrow(max)>0){
    ym.max <- unique(max[,c("V2", "V3")])
    
    process.max <- data.frame(Station = mytemp_sort$station[1],
                              Year = rep(ym.max[,1], each = 31),
                              Month = rep(ym.max[,2], each = 31),
                              Day = 1:31,
                              Tmax = (max[,4:34] %>% as.matrix %>% t %>% as.vector) / 10
    )}
  
  if(exists("process.max") & exists("process.min")) {
    temp.all <- merge(process.min, process.max, all = TRUE)
    all <- temp.all %>% mutate(Tavg =rowMeans(temp.all[c("Tmin", "Tmax")], na.rm=TRUE)) 
  } else if (exists("process.avg")){
    all <- process.avg %>% mutate(Tmin = NA, Tmax = NA)
  } else {all <- data.frame(Station = mytemp_sort$station[1], Tavg = NA, Tmin = NA, Tmax = NA, prcp = NA)}
  
  if(exists("process.prcp")) {
    all <- merge(all, process.prcp)
  }
  
  
  return(all)
}
