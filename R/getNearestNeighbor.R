#' Get Nearest Neighbors
#'
#' This is a FAST function that identifies the nearest neighbor of each individual
#' and the median pairwise distance between them over an overlapping tracking
#' period during some subset of daily averaged caribou locations (e.g. output of
#' \link{getDailyMean}). All pairwise combinations of individuals are compared; pairs are
#' matched by calendar day (\code{yday}) and year. Pairs with insufficient
#' overlapping days or whose median separation exceeds a maximum distance
#' threshold are excluded from the result.
#'
#' @param x a data frame or simple feature (\code{sf}) object containing
#'   projected X, Y locations, typically the output of \link{getDailyMean}.
#'   Must contain either projected coordinates or an \code{sf} geometry column.
#' @param id.col Name of the column containing individual animal
#'   identifiers. Default \code{"ID"}.
#' @param time.col Name of colum containing POSIX date and time for extracting 
#'   Year and yday as needed. Default \code{"Time"}. 
#' @param min_days  Minimum number of co-located days (matched
#'   \code{yday} and \code{Year}) required to include a pair in the output.
#'   Pairs with fewer overlapping observations are dropped. Default \code{10}.
#' @param min_distance  Threshold distance (in km) between centroids of all 
#'   paired locations, to avoid measuring day-by-day distances of animals many
#'   hundreds of km apart. Default 1000km (i.e. most animals retained). 
#' @return a data frame with one row per individual, containing:
#'   \describe{
#'     \item{\code{ID}}{individual animal identifier}
#'     \item{\code{Neighbor}}{identifier of the closest individual,
#'       defined as the one with the lowest median pairwise distance over
#'       overlapping days}
#'     \item{\code{D_median}}{median distance (in metres) to the nearest
#'       neighbour over all co-located days; \code{NA} if the nearest
#'       neighbour exceeds \code{min_distance} km}
#'   }
#'
#' @seealso \link{getDailyMean}
#' @example examples/example_getNearestNeighbor.R

getNearestNeighbor <- function(x, 
                               id.col = "ID", time.col = "Time", 
                               min_days = 10,
                               min_distance = 1e3){
    
    # 1. Check projection if sf
    if(inherits(x, "sf")) {
        crs <- st_crs(x)
        if(is.na(crs) || crs$IsGeographic)
            stop("x must be in a projected (planar) CRS; X and Y must be on the same scale for distances.")
        x_df <- data.frame(x, st_coordinates(x)) |>
            mutate(Z = X + 1i*Y, geometry = NULL,
                   ID = get(id.col),
                   Time = get(time.col))
    } else {
        # 2. Check X, Y present if data frame
        if(!all(c("X","Y") %in% names(x)))
            stop("x must contain columns 'X' and 'Y' with projected coordinates.")
        x_df <- x |> mutate(Z = X + 1i*Y,
                            ID = get(id.col),
                            Time = get(time.col))
    }
    
    # 3. Extract yday, check Year present
    if(!"Year" %in% names(x_df))
        stop("x must contain a 'Year' column.")
    if(!"yday" %in% names(x_df))
        x_df$yday <- as.integer(format(as.Date(x_df$Time), "%j"))
    
    # 4. Check unique yday/Year/ID combinations
    dups <- duplicated(x_df[, c("ID", "Year", "yday")])
    if(any(dups))
        warning(sum(dups), " duplicate ID/Year/yday combinations found; only first retained.")
    x_df <- x_df[!dups, ]
    
    x_split <- dlply(x_df, id.col)
    
    x_split <- dlply(x_df, id.col)
    
    # all pairs
    pairs <- combn(names(x_split), 2, simplify = FALSE)
    cat("Pairs before filter:", length(pairs), "\n")
    centroids <- ldply(x_split, function(d) 
        data.frame(Z_centroid = mean(d$Z, na.rm = TRUE)))
    
    # Filter pairs by centroid distance before computing full pairwise distances
    pairs <- Filter(function(p) {
        Mod(centroids$Z_centroid[centroids$.id == p[1]] - 
                centroids$Z_centroid[centroids$.id == p[2]]) < (min_distance * 1e3)
    }, pairs)
    cat("Pairs after filter:", length(pairs), "\n")

    valid_pairs <- data.table(
        ID1 = sapply(pairs, `[`, 1),
        ID2 = sapply(pairs, `[`, 2)
    )
    
    x_dt <- as.data.table(x_df[, c("ID", "yday", "Year", "Z")])
    
    pair_dists <- rbindlist(lapply(split(x_dt, by = c("Year", "yday")), function(d) {
        if(nrow(d) < 2) return(NULL)
        
        # Full distance matrix for all animals present this day
        ids <- d$ID
        D <- outer(d$Z, d$Z, function(z1, z2) Mod(z2 - z1))
        rownames(D) <- colnames(D) <- ids
        
        # Subset to valid pairs only
        valid_pairs[ID1 %in% ids & ID2 %in% ids][, 
                                                 .(ID1, ID2, 
                                                   yday = d$yday[1], 
                                                   Year = d$Year[1],
                                                   D = D[cbind(ID1, ID2)])]
    }))
    
    # summarize
    
    distance_summaries <- pair_dists[, .(D_median = median(D, na.rm = TRUE),
                                         n_pairs  = .N), 
                                     by = .(ID1, ID2)][n_pairs > min_days]
    d_closest <- ddply(distance_summaries, "ID1", 
                       summarize, 
                       Neighbor = ID2[which.min(D_median)],
                       D_median = min(D_median)) |> plyr::rename(c(ID1 = "ID")) 
    
    # double for all pairs
    
    d_closest <- rbindlist(list(
        d_closest,
        setNames(d_closest[, c("Neighbor", "ID", "D_median")], 
                 c("ID", "Neighbor", "D_median"))))
    
    return(d_closest)
}
