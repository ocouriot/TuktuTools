#' Remove almost duplicated timestamp
#'
#' Function to remove almost duplicated timestamp.
#' It appears that sometimes, the GPS device relocate at 1 or 2
#' minute interval. However, the GPS devices are configured to
#' relocate every day, 8-hours or every hour for some
#' individuals, at fix hours (e.g. 0:00 am, 8:00 am or 4:00 pm).
#' In addition, it is biologically impossible that the animal
#' moves several kilometers in one minute. Thus, the relocations
#' having a time interval in the order of minutes or very high
#' movement rates are more prone
#' to be 'outliers'.
#'
#' I noted that sometimes there could be several subsequent "outliers"
#' thus the cleaning function is repeated several times until there are no "outliers" anymore

#' @param df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (in a metric system)
#' Time: vector (of class POSIXct)
#' @param steps if specified, the number of cleaning steps to be performed (default is 10)
#'
#' @return The function returns adds a column "outlier". If TRUE, it means that 
#' the location has been identified as an outlier.
#' @example examples/example_removeOutliers.R
#'
#' @export

removeOutliers <- function(df, steps = 10){

  # Function for Cleaning dataset
cleanData <- function(dfa){
  
  dfa <- dfa %>% arrange(ID, Time)
  row.names(dfa) <- 1:nrow(dfa)

    tempo.speed <- ddply(dfa, c("ID", "Year"), getSpeed) %>% mutate(dt.sec = dt*24*60)
    
    # relocations with high speed (>50km per hour) or small time interval (<= 2 minutes) or with high speed & small time interval (20km per hour and dt<10 minutes)
    table(tempo.speed$speed > 50000 | tempo.speed$dt.sec <= 120 | 
            (tempo.speed$speed > 20000 & tempo.speed$dt.sec <= 600))
    flagged <- which(tempo.speed$speed > 50000 | tempo.speed$dt.sec <= 120 | 
                       (tempo.speed$speed > 20000 & tempo.speed$dt.sec <= 600))
    
    # flag outliers
    if(length(flagged) != 0){
      dfa$outlier[flagged] <- "TRUE"
    }

    return(dfa)
}

#################
## Apply the function on the dataset

# create a unique identifier for each individual per Year
c1 <- subset(df %>% as.data.frame, ! ID %in% names(which(table(df$ID)<3)))
keep <- c1 %>% mutate(outlier = "FALSE")

# Loop to clean data in several steps, once there are no outliers left,
# the loop stop and return the dataframe without outliers
toremove <- data.frame()
for(j in 1:steps){
  # look for potential "outliers"
  c1.speed <- keep %>% subset(outlier == "FALSE") %>% ddply(c("ID","Year"), getSpeed) %>% 
    mutate(dt.sec = dt * 24 * 60)
  
  
  # see if there are some outliers: speed > 15km per hour, or delta time < 2 min, or speed > 10 km per hour and delta time < 10 minutes
  verif <- table(c1.speed$speed > 50000 | c1.speed$dt.sec <= 120 | 
                   (c1.speed$speed > 20000 & c1.speed$dt.sec <= 600))
  if(j < steps){
    if(dim(verif)==2){
      ### CLEAN DATA
      print(paste0("Cleaning Step ",j))
      dfb <- keep %>% subset(outlier == "FALSE") %>% ddply(c("ID", "Year"), cleanData)
      print(paste0("Number of 'outliers' detected: ", length(dfb$outlier[dfb$outlier == "TRUE"])))
      print(" ")
      keep$outlier[as.numeric(row.names(dfb[dfb$outlier =="TRUE",]))] <- "TRUE"
    } else if((dim(verif)==1 & names(verif)==FALSE)==TRUE){
      if(class(df)[1] == "sf"){
        keep <- st_as_sf(keep, crs = st_crs(df))
        }
      return(keep)}
    } # END IF j < steps
  if(j == steps){
    print(paste0("Cleaning Step ",j))
    dfb <- keep %>% subset(outlier == "FALSE") %>% ddply(c("ID", "Year"), cleanData)
    print(paste0("Number of 'outliers' detected: ", length(dfb$outlier[dfb$outlier == "TRUE"])))
    print(" ")
    keep$outlier[as.numeric(row.names(dfb[dfb$outlier =="TRUE",]))] <- "TRUE"
    if(class(df)[1] == "sf"){
      keep <- st_as_sf(keep, crs = st_crs(df))
    }
    return(keep)
  } # end IF j == steps

} # END FOR loop

return(keep)
}
