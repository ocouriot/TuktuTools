# using default caribou dataset as example

data(caribou)
caribou.sf <- st_as_sf(caribou |> mutate(Lat = Lat, Lon = Lon), 
                       coords = c("Lon","Lat")) |> st_set_crs(4326)

data(caribou)
c_neighbor <- rbind(caribou |> subset(ID == "Dancer" & Year == 2004),
                    caribou |> subset(ID == "Prancer" & Year == 2015),
                    caribou |> subset(ID == "Vixen" & Year == 2008),
                    caribou |> subset(ID == "Comet" & Year == 2007))

caribou_dailymean <- getDailyMean(c_neighbor) |> 
    ddply("ID", subset, Year == Year[1]) |> 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
    mutate(Year = 2020) |>   # coercing to one year 
    st_transform(3978)       # projecting to Canada lambert

getNearestNeighbor(caribou_dailymean)


# Using some real NWT data

    require(TuktuData)
    data("nwt_raw")
    data("Canada_lambert")
    
    c <- nwt_raw |> subset(Year == 2023 & month(Time) == 4 & sex == "f") |> 
        getDailyMean_dt(id.col = "OriginalID") |> st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |> 
        st_transform(Canada_lambert)
    c_nn <- getNearestNeighbor(c, id.col = "OriginalID", min_distance = 500)
    
# plot results:
   
    hist(c_nn$D_median, breaks = 50)
    