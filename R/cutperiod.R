#' Cut a timeseries for the period of interest
#'
#' Function to prepare and clean the data by keeping the period of interest.
#' We first have to keep GPS locations for the period of interest:
#' for example, for the calving period:
#' Following Cameron et al. 2018, we defined the calving period from
#' May 19 to July 7
#' In addition, the parturition model seems quite accurate, but if some individuals
#' have lost signal for several days it could lead to a undetermination
#' of the calving event.
#'
#' Before running the parturition model, we have to establish rules
#' to determine the individuals that will be included in the analysis
#' and the ones that will be excluded
#' For example:
#' -exclude individuals with less than 1 fix per day
#' -exclude individuals that lost signal for more than 3 days
#' -exclude individuals for which monitoring stopped before July 15 or began after May 19

#' @param  df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (in N Canada Lambert Conformal Conic)
#' Time: date and time vector of the relocation (of class POSIXct)
#' @param start starting month and day of the period of interest as a vector c(mm,dd)
#' (example for the 19th of May: c(05,19))
#' @param end end month and day of the period of interest as a vector c(mm,dd)
#' (example for the 7th of July: c(07,07))
#' @param nfixes minimun number of fixes per day
#' (taking the average number of fixes per day for each individual across the period of interest)
#' @param dayloss maximum number of days with missing locations
#' (for example, if an individual has loss signal for more than 3 consecutive days, it will be excluded from the dataset)
#' @param CRS the coordinates projection (default is Canada Lambert Conformal Conic centered on the area from the Western Arctic to the Hudson Bay:
#' "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
#' @return a dataframe containing only concerned period and individuals that match the defined rules
#' 
#' @example examples/example_cutperiod.R
#' 
#' 
#' @export

cutperiod <- function(df, start, end, nfixes, dayloss,
                      CRS = "+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"){
  months <- (start[1]+1):(end[1]-1)
  nbegin <- length(unique(df$ID))

# just keep time series between defined start and end
tempo <- droplevels(df %>% mutate(Year = year(Time), Month = month(Time), Day = day(Time)) %>% mutate(ID_Year = as.factor(paste(ID, Year, sep = "_"))) %>%
                         subset((Month == start[1] & Day >= start[2])| Month %in% months | (Month == end[1] & Day <= end[2])))


# Remove individuals for which monitoring stopped before July 15th
# or began after May 19th
# calculate start and end date of the monitoring for each individual of each herd
df <- merge(tempo,with(tempo, aggregate(list(start.date = Time),
                                                   list(ID_Year = ID_Year),min)),
                 by="ID_Year",all=T)
df <- merge(df,with(tempo, aggregate(list(end.date = Time),
                                                  list(ID_Year = ID_Year),max)),
                 by="ID_Year",all=T)
# filtrate
Start <- ymd(paste0(df$Year,rep(paste0("-",start[1],"-",start[2]),nrow(df))))+1
End <- ymd(paste0(df$Year,rep(paste0("-",end[1],"-",end[2]),nrow(df))))-1
df <- droplevels(subset(df,start.date <= Start &
                          end.date >= End))

# verify loss of individuals
length(unique(tempo$ID_Year));length(unique(df$ID_Year))

# calculate the dt between successive relocations for each individual
get.dt <- function(dfa){
  dfa <- droplevels(subset(dfa,! ID_Year %in% names(which(table(dfa$ID_Year)<3))))
  keep <- data.frame()
  for (i in unique(dfa$ID_Year)){
#    print(length(unique(dfa$ID_Year))-which(unique(dfa$ID_Year)==i))
    temp <- droplevels(subset(dfa,ID_Year == i))
    temp$dt <- c(as.integer(difftime(temp$Time[2:length(temp$Time)],
                                      temp$Time[1:(length(temp$Time)-1)],"hours")),NA)
    keep <- rbind(keep,temp)
  }
  return(keep)
}

# run the function
df <- get.dt(df)


# exclude individuals that do not have at least one fix per day
df$yday <- yday(df$Time)
df.freq.ind <- with(df,aggregate(list(freq=ID),
                                 list(ID_Year=ID_Year, yday = yday), length))
mean.freq.ind <- with(df.freq.ind,aggregate(list(mean.freq=freq),
                                   list(ID_Year=ID_Year), mean, na.omit=TRUE))
indtokeep <- unique(droplevels(subset(mean.freq.ind, mean.freq > nfixes))$ID_Year)
tempo <- droplevels(subset(df, ID_Year %in% indtokeep))


# now exclude individuals that have lost signal for more
# than 'dayloss' consecutive days during the calving period
df.dayloss <- droplevels(subset(tempo, dt > dayloss*24))
inddayloss <- unique(df.dayloss$ID_Year)
df <- droplevels(subset(tempo, ! ID_Year %in% inddayloss))
attr(df, "projection") <- CRS

# how many individuals have been removed?
print(paste0("Period comprised between ", month(start[1],label=T)," ", start[2]," and ", month(end[1],label=T)," ", end[2]))
print(paste0("Number of excluded individuals: ",nbegin-length(unique(df$ID))))

return(df %>% mutate(ID_Year = NULL, Year = NULL, Mont = NULL, Day = NULL))
}



