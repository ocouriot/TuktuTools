require(TuktuTools)
data(caribou)

# plyr version

distances <- caribou |> ddply(c("ID", "Year"), computeDistances, 
                              X.col = "x", Y.col = "y", start = "02-01", end  = "06-01")

# dplyr version

distances <- caribou |>
    group_by(ID, Year) |>
    group_modify(~ computeDistances(.x, X.col = "x", Y.col = "y",
                                    start = "02-01", end = "06-01")) |>
    ungroup()