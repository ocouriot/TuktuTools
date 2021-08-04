#' Get migration arrival from calving ground
#'
#' Function to determine migration arrival from the calving ground
#' (estimated based on the individuals'calving locations).
#'
#' The function estimates the calving ground of each herd, based on calving locations sepecified given in df
#' Then it determines the migration arrival time as the time at which a given individual crossed the boundaries of the herd
#' calving ground (i.e. the first relocation inside the calving range)

#' @param df a dataframe containing the ID_Year, the herd and lon and lat of the calving event (in WGS 84)
#' @param movedata a simple feature containing the movement data for all individuals in df, in WGS84 (crs=4326) (with column ID_Year and DateTime of the relocation)
#' @param percent the percent to be used to estimate the calving ground
#' @param method the method to be used to estimate the calving ground of the herd (prefrerred 'LSCV')
#'
#' @return a dataframe with the ID_Year and the day the individual crossed the boundaries
#' of the calving ground (estimated based on the method and percent defined in the parameters)
#'
#'
#' @export


getCrossingTime <- function(df, movedata, percent, method, 
                            CRS='+proj=lcc +lat_1=50 +lat_2=70 +lat_0=65 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'){
  p.sf <- st_as_sf(df, coords = c("lon","lat"), crs = 4326) %>% st_transform(crs=CRS)
  p.xy <- st_coordinates(p.sf)

  # my.sf <- subset(p.sf, herd == "Bluenose West")
  calvingrange <- list()
  for(myherd in levels(p.sf$herd)){
    calvingrange[[length(calvingrange)+1]] <- getKernelUD(subset(p.sf, herd == myherd),
                                                          h = method, percent = percent)
  }

  c.df<-ldply(calvingrange) %>% st_as_sf(crs = CRS)

  c.df$id <- levels(p.sf$herd)

  # intersect movement data with the calving ranges to
  # determine the arrival time for each female

  tot <- movedata %>% mutate(yday = yday(DateTime))
  tot <- subset(tot, ID_Year %in% unique(df$ID_Year)) %>%  subset(yday >= 91 & yday <= 188)

  tot <- merge(tot, df[,c('ID_Year','herd')], by = 'ID_Year', all.x = TRUE)
  tot <- st_transform(tot, crs = CRS)
  head(tot)
  
  arrival_dates <- data.frame()
  for (myherd in c.df$id){
    print(myherd)
    range <- subset(c.df, id == myherd)
    points <- subset(tot, herd == myherd)
    intersect <- st_intersection(points, range)
    arrivals <- with(intersect, aggregate(list(crossing.day = yday), list(ID_Year = ID_Year), min))
    arrival_dates <- rbind(arrival_dates, arrivals)
  }
  return(arrival_dates)
}
