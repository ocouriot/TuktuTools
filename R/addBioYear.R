#' Add Biological Year to data
#'
#' This function adds a column designating biological year starting on a user-designated date 
#' e.g. 1 June (day of year 152) for caribou
#'
#' @param df a data frame with columns:
#' Time: Date and time in POSIXct
#' @param bioStart biological year start date, in day of year format \code{\link{yday}}
#'
#' @return a data frame with columns:
#' bioYear: biological year corresponding to user-defined start day}
#' 
#'
#' @example examples/example_addBioYear.R
#' @export

addBioYear <- function(df, bioStart){
  bioYear = ifelse(leap_year(df$Time) == FALSE & (yday(df$Time) < bioStart), year(df$Time) -1, 
                   ifelse(leap_year(df$Time) & (yday(df$Time) < bioStart+1), year(df$Time) -1, year(df$Time)))
  df1 <- mutate(df,
                bioYear = bioYear)
  return(df1)
  
}

