#' Add Biological Year to data
#'
#' This function adds a column designating biological year starting on a user-designated date 
#' e.g. 1 June (day of year 152) for caribou
#'
#' @param df a dataframe with columns:
#' \describe{
#'   \item{DateTime}{Date and time in POSIXct}
#' @param bioStart biological year start date, in day of year format \code{\link{yday}}
#'
#' @return a dataframe with columns:
#' \describe{
#'   \item{bioYear}{Biological year corresponding to user-defined start day}
#' }
#'
#' @example examples/example_addBioYear.R

addBioYear <- function(df, bioStart){
  bioYear = ifelse(yday(df$DateTime) < bioStart, year(df$DateTime) -1, year(df$DateTime))
  df1 <- mutate(df,
                bioYear = bioYear)
  return(df1)
  
}
