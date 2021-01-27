#' Get migration arrival from calving ground
#'
#' Function to determine migration arrival from the calving ground
#' (estimated based on the individuals'calving locations).
#'
#' The function estimates the calving ground of each herd, based on calving locations sepecified given in df
#' Then it determines the migration arrival time as the time at which a given individual crossed the boundaries of the herd
#' calving ground (i.e. the first relocation inside the calving range)

#' @param df a dataframe containing the ID_Year, the herd and lon and lat of the calving event (in WGS 84)
#' @param percent the percent to be used to estimate the calving ground
#' @param method the method to be used to estimate the calving ground of the herd (prefrerred 'LSCV')
#'
#' @return a dataframe with the ID_Year and the day the individual crossed the boundaries
#' of the calving ground (estimated based on the method and percent defined in the parameters)
#'
#'
#' @export


getCrossingTime <- function(df, percent, method){
  p.sf <- st_as_sf(df, coords = c("lon","lat"), crs = 4326) %>% st_transform(3857)
  p.xy <- st_coordinates(p.sf)

  # my.sf <- subset(p.sf, herd == "Bluenose West")
  calvingrange <- list()
  for(myherd in levels(p.sf$herd)){
    calvingrange[[length(calvingrange)+1]] <- getKernelUD(subset(p.sf, herd == myherd),
                                                          h = method, percent = percent)
  }

  c.df<-ldply(calvingrange) %>% st_sf

  c.df$id <- levels(p.sf$herd)

  c.df <- c.df %>% st_transform(4326)

  # get the relocations of individuals and intersect with the calving ranges to
  # determine the arrival time for each female
  data(wah);data(bluenose);data(bathurst);data(inuvik);data(beverly)

  tot <- rbind.fill(wah, bluenose, bathurst, inuvik, beverly) %>%
    mutate(ID_Year = paste(ID, Year, sep='_'), yday = yday(DateTime)) %>% st_as_sf
  tot <- subset(tot, ID_Year %in% unique(df$ID_Year)) %>% st_transform(4326) %>%
    subset(yday >= 91 & yday <= 188)

  head(tot)

  tot <- merge(tot, df[,c('ID_Year','herd')], by = 'ID_Year', all.x = TRUE)

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
