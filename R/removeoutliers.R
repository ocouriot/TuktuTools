#' Remove almost duplicated timestamp
#'
#' Function to remove almost duplicated timestamp.
#' It appears that sometimes, the GPS device relocate at 1 or 2
#' minute interval. However, the GPS devices are configured to
#' relocate every day, 8-hours or every hour for some
#' individuals, at fix hours (e.g. 0:00 am, 8:00 am or 4:00 pm).
#' In addition, it is biologically impossible that the animal
#' moves several kilometers in one minute. Thus, the relocations
#' having a time interval in the order of minutes are more prone
#' to be 'outliers'.
#'
#' I noted that sometimes there could be several subsequent "outliers"
#' thus the cleaning function is repeated several times until there are no "outliers" anymore

#' @param df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (metric system specified in CRS)
#' DateTime: vector (of class POSIXct)
#' @param steps if specified, the number of cleaning steps to be performed (default is 10)
#' @param CRS the coordinates projection (default is Canada Lambert Conformal Conic:
#' "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
#'
#' @return a list with a dataframe without potential 'outliers' and a dataframe of outliers

#'
#' @export

removeOutliers <- function(df, steps = 10,
                           CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"){

  df <- as.data.frame(df)
# Function for Cleaning dataset
clean.data <- function(dfa){
  dfa <- dfa[order(dfa$ID_Year,dfa$DateTime),]
  row.names(dfa) <- 1:nrow(dfa)
  newdata <- data.frame()
  outliers <- data.frame()

    tempo.speed <- get.speed(dfa)
    tempo.speed$dt.sec <- tempo.speed$dt * 24 * 60

    # relocations with high speed (>15km per hour) or small time interval (<= 2 minutes) or with high speed & small time interval (10km per hour and <10 minutes)
    table(tempo.speed$speed > 15000 | tempo.speed$dt.sec <= 120 | (tempo.speed$speed > 10000 & tempo.speed$dt.sec <= 600))
    rowstokeep <- which(tempo.speed$speed > 15000 | tempo.speed$dt.sec <= 120 | (tempo.speed$speed > 10000 & tempo.speed$dt.sec <= 600))
    # remove the location i+1 (since dt and speed are calculated between the loc i and i+1, the "outlier" is loc i+1)
    rowstoremove <- rowstokeep + 1

    if(length(rowstoremove) != 0){
      dfb <- droplevels(dfa[-rowstoremove,])
      remove <- droplevels(dfa[rowstoremove,])
    } else {
      dfb <- dfa
      remove <- dfa[0,]
    } # END if statement

    rm(tempo.speed, rowstokeep,rowstoremove)

    newdata <- dfb
    outliers <- remove
  return(list(newdata=newdata, outliers=outliers))
}

#################
## Apply the function on the dataset


# create Month, Day, Year variables
df$Month <- month(df$DateTime);df$Day <- day(df$DateTime);df$Year <- year(df$DateTime)

head(df)

# create a unique identifier for each individual per Year
df$ID_Year <- as.factor(paste(df$ID, df$Year, sep="_"))

c1 <- droplevels(subset(df, ! ID_Year %in% names(which(table(df$ID_Year)<3))))
keep <- c1

# Loop to clean data in several steps, once there are no outliers left,
# the loop stop and return the dataframe without outliers
toremove <- data.frame()
for(j in 1:steps){
# look for potential "outliers"
c1.speed <- get.speed(keep)

# calculate the delta time in seconds
c1.speed$dt.sec <- c1.speed$dt * 24 * 60

# see if there are some outliers: speed > 15km per hour, or delta time < 2 min, or speed > 10 km per hour and delta time < 10 minutes
verif <- table(c1.speed$speed > 15000 | c1.speed$dt.sec <= 120 | (c1.speed$speed > 10000 & c1.speed$dt.sec <= 600))
if(j < steps){
  if(dim(verif)==2){
    ### CLEAN DATA
    print(paste0("Cleaning Step ",j))
    dfb <- clean.data(keep)
    print(paste0("Number of removed 'outliers': ", dim(keep)[1]-dim(dfb$newdata)[1]))
    print(" ")
    keep <- dfb$newdata
    toremove <- rbind(toremove,dfb$outliers)
  } else if((dim(verif)==1 & names(verif)==FALSE)==TRUE){
    no.outliers <- keep
    attr(no.outliers, "projection") <- CRS
    return(toreturn=list(no.outliers=no.outliers,outliers=toremove))} else {
      print("Check data")}
  }
if(j == steps){
  no.outliers <- keep
  attr(no.outliers, "projection") <- CRS
  return(toreturn=list(no.outliers=no.outliers,outliers=toremove))
}

}

return(toreturn)
}
