#' Remove (near) duplicated timestamp
#'
#' Function to remove almost duplicated timestamps. Occasionally the GPS device 
#' transmits locations at 1 or 2 minute intervals despite the duty cycle being 
#' set to 1 day, 8 hours, or - at most - 2 hours.  This leads notably to highly 
#' unrealistic speeds (e.g. several kilometers in one minute). This function 
#' flags locations with very short intervals or unrealistic speeds as outliers.  

#' @param df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (in a metric system)
#' Time: vector (of class POSIXct)
#' @param steps if specified, the number of cleaning steps to be performed (default is 10)
#' @param max.speed threshold (in km/hour) for removing outliers.  Default: 50 km/hr
#' @param min.interval threshold (in minutes) for mininum intervals between location. Default: 2 min.
#' 
#' @return The original data frame with an additional column labeled "outlier". 
#' @example examples/example_removeOutliers.R
#'
#' @export

removeOutliers <- function(df, steps = 10, max.speed = 50, min.interval = 2){

  # Function for Cleaning dataset
cleanData <- function(dfa){
  
  dfa <- dfa %>% arrange(ID, Time)
  if(!("Year" %in% names(dfa))) {dfa$Year = lubridate::year(dfa$Time)}
  row.names(dfa) <- 1:nrow(dfa)

    tempo.speed <- ddply(dfa, c("ID", "Year"), getSpeed) %>% mutate(dt.sec = dt*60*60)
    
    # relocations with high speed (>50km per hour) or small time interval 
    # (<= 2 minutes) or with high speed & small time interval 
    # (20km per hour and dt<10 minutes)
    
    flagged <- which(tempo.speed$speed > max.speed*1e3  | tempo.speed$dt.sec <= min.interval*60 | 
                       (tempo.speed$speed > 20000 & tempo.speed$dt.sec <= 600))
    
    # flag outliers
    if(length(flagged) != 0){
      dfa$outlier[flagged] <- TRUE
    }

    return(dfa)
}

#################
## Apply the function on the dataset

# create a unique identifier for each individual per Year
c1 <- subset(df %>% as.data.frame, ! ID %in% names(which(table(df$ID)<3)))
keep <- c1 %>% mutate(outlier = FALSE)

# Loop to clean data in several steps, once there are no outliers left,
# the loop stop and return the dataframe without outliers
toremove <- data.frame()
for(j in 1:steps){
  # look for potential "outliers"
  c1.speed <- keep %>% subset(!outlier) %>% ddply(c("ID","Year"), getSpeed) %>% 
    mutate(dt.sec = dt * 24 * 60)
  
  
  # see if there are some outliers: speed > 15km per hour, or delta time < 2 min, or speed > 10 km per hour and delta time < 10 minutes
  verif <- table(c1.speed$speed > 50000 | c1.speed$dt.sec <= 120 | 
                   (c1.speed$speed > 20000 & c1.speed$dt.sec <= 600))
  if(j < steps){
    if(dim(verif)==2){
      ### CLEAN DATA
      cat(paste0("Cleaning Step ",j, "\n"))
      dfb <- keep %>% subset(!outlier) %>% ddply(c("ID", "Year"), cleanData)
      cat(paste0("Number of 'outliers' detected: ", length(dfb$outlier[dfb$outlier]), "\n"))
      keep$outlier[as.numeric(row.names(dfb[dfb$outlier,]))] <- TRUE
    } else if((dim(verif)==1 & names(verif)==FALSE)==TRUE){
      if(class(df)[1] == "sf"){
        keep <- st_as_sf(keep, crs = st_crs(df))
        }
      return(keep)}
    } # END IF j < steps
  if(j == steps){
    cat(paste0("Cleaning Step ",j, "\n"))
    dfb <- keep %>% subset(!outlier) %>% ddply(c("ID", "Year"), cleanData)
    cat(paste0("Number of 'outliers' detected: ", length(dfb$outlier[dfb$outlier]), "\n"))
    
    keep$outlier[as.numeric(row.names(dfb[dfb$outlier,]))] <- TRUE
    if(class(df)[1] == "sf"){
      keep <- st_as_sf(keep, crs = st_crs(df))
    }
    return(keep)
  } # end IF j == steps

} # END FOR loop

return(keep)
}
