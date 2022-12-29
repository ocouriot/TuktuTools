#' Process a timeseries for the period of interest
#'
#' Processes and filters caribou movement data by 
#' (a) trimming to analysis period of interest; for example, in the parturition analysis
#' dates were limited to May 19 to July 7 ; 
#' (b) guaranteeing a minimum number of location fixes a day (e.g. 1 for the parturiton analysis), and removing 
#' individuals that have too few; and 
#' (c) removing individuals with a data gap greater than some minimum threshold (e.g. 3 days for the parturiton analysis). 
#' 
#' @param  df a data frame containing columns: ID as individual identifiant,
#' x and y: relocations of individuals (in N Canada Lambert Conformal Conic)
#' Time: date and time vector of the relocation (of class POSIXct)
#' @param start starting date of the period of interest as a character in the form "mm-dd"
#' (example for the 19th of May: "05-19")
#' @param end end date of the period of interest as a character in the form "mm-dd" (excluded)
#' (example for the 8th of July: "07-08")
#' @param nfixes individuals with fewer than nfixes observations per day over the period of of interest are removed
#' @param dayloss maximum number of consecutive days with missing locations
#' (for example, if an individual has loss signal for more than 3 consecutive days, 
#' it will be excluded from the dataset)
#' @return a processed dataframe
#' 
#' @example examples/example_prepData.R
#' 
#' 
#' @export

prepData <- function(df, start, end, nfixes = Inf, dayloss = Inf, restrictive = FALSE){

    # just keep time series between defined start and end
  tempo <- df %>% as.data.frame %>% 
    mutate(Year = year(Time), 
           start = ymd(paste(Year, start), tz = tz(Time)),
           end = ymd(paste(Year, end), tz = tz(Time))) %>%
    subset(Time >= start & Time < end) %>% 
    mutate(start = as.Date(start), end = as.Date(end))
  
  # Remove individuals for which monitoring stopped before the end or 
  # began after the start
  if(restrictive){
    tempo <- tempo %>% ddply(c("ID", "Year"), function(x) x %>% 
                                mutate(start.monitoring = as.Date(min(.$Time)), 
                                       end.monitoring = as.Date(max(.$Time)))) %>% 
      subset(start.monitoring == start & end.monitoring == end) %>% 
      mutate(ID_Year = as.factor(paste(ID, Year, sep ="_")))
  }

  
  # calculate the dt between successive relocations for each individual and exclude 
  # those with less fixes per day than nfixes (e.g., 1 for parturition) and missing data for more than
  # dayloss (e.g., 3 for parturition) consecutive days
  
  tempo2 <- tempo %>% 
    ddply(c("ID", "Year"), 
          function(x) x %>% 
            arrange(Time) %>% 
            mutate(dt = c(NA, as.integer(difftime(Time[2:length(Time)],Time[1:(length(Time)-1)],"hours"))))) %>%
    ddply(c("ID", "Year"), 
          function(x) x %>% 
            mutate(meandt = mean(.$dt, na.rm = TRUE), maxdt = max(.$dt, na.rm = TRUE))) %>% 
      subset(meandt < nfixes*24 & maxdt < dayloss*24) %>% droplevels %>% 
    mutate(start = NULL, end = NULL, start.monitoring = NULL, end.monitoring = NULL, 
           ID_Year = NULL, dt = NULL, meandt = NULL, maxdt = NULL)
  
  # how many individuals have been removed?
  cat(paste0("Period clipped to ", start," - ", end, "\n"))
  cat(paste0("Number of excluded individuals-years: ",
             length(unique(paste0(df$ID,df$Year)))-
               length(unique(paste0(tempo2$ID,tempo2$Year))),
             "\n"))
  
  if(class(df)[1] == "sf"){
    tempo2 <- st_as_sf(tempo2, crs = st_crs(df))
  }
  return(tempo2)
}


