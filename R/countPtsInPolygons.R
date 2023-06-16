
#' Count Points in polygons
#'
#' This function takes each animal ID and counts how many of its locations are 
#' in polygon boundaries. This is helpful to help decide whether the study area 
#' an animal was assigned to is accurate or to identify which region an animal 
#' spends most of its time.
#'
#' @param sf simple feature points with columns:
#' ID: designating individuals
#' Time: Date and time in POSIXct
#' @param assigned_col: study area an animal was assigned to (likely from movebank)
#'
#' @param bounds simple features multipolygon  with column:
#' @param bounds_col: identifying unique polygon boundaries
#' 
#' @return a data frame with columns:
#' ID: animal ID
#' start: first date of monitoring
#' end: last date of monitoring
#' n_pts: total number of locations
#' assigned: study area animal was originally assigned
#' columns for each polygon boundary with number of locations
#' 
#' @example examples/example_getMCP.R
#' 
#' @export
#' 

countPtsInPolygons <- function(sf, bounds, id_col, assigned_col = "study_site"){
  
  bounds <- bounds %>% data.frame %>% 
    rename("id_col" = id_col) %>%
    st_as_sf
  
  pts <- st_join(sf, bounds) %>% rename("assigned" = assigned_col)
  
  pts$id_col[is.na(pts$id_col)] <- "out"
  
  pts$id_col <- do.call(recode,
                        c(list(pts$id_col),
                          setNames(vctrs::vec_as_names(unique(pts$id_col), 
                                                       repair = "universal"),
                                   unique(pts$id_col))))
  
  pts_wide <- pts[,c("ID", "Time", "assigned", "id_col")] %>% st_drop_geometry %>% 
    mutate(present = 1) %>%
    tidyr::pivot_wider(names_from = id_col, values_from = present, values_fill = 0)
  
  pts_summary <- pts_wide %>% group_by(ID) %>%
    mutate(start = min(Time), end = max(Time),
           n_pts = n(),
           assigned = first(assigned)) %>%
    select(-Time) %>%
    group_by(ID, start, end, n_pts, assigned) %>%
    summarise_all(sum) %>%
    arrange(ID, assigned, start)
}
