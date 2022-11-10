#' Add Biological Year to data
#'
#' This function adds a column designating biological year starting on a user-designated date 
#' e.g. 1 June (day of year 152) for caribou
#'
#' @param df a dataframe with columns:
#' \describe{
#'   \item{Time}{Date and time in POSIXct}
#' @param bioStart biological year start date, in day of year format \code{\link{yday}}
#'
#' @return a dataframe with columns:
#' \describe{
#'   \item{bioYear}{Biological year corresponding to user-defined start day}
#' }
#'
#' @example examples/example_addBioYear.R
#' @export

addBioYear <- function(df, bioStart){
  bioYear = ifelse(yday(df$Time) < bioStart, year(df$Time) -1, year(df$Time))
  df1 <- mutate(df,
                bioYear = bioYear)
  return(df1)
  
}

