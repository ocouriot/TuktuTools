#' Get herd assignment using spatial clustering
#'
#' Function to determine herd assignment using K-means analysis on GPS locations of individuals,
#' using the average location for each individual and year.
#'
#' @param df a dataframe containing x and y coordinates and the ID_year
#' (Identifiant of individuals for each year of survey) of all datasets combined in one
#' @param K number of herds (can be determined using getKnumber function in this package)
#' @param plotIt if TRUE, plot the GPS location with herd assignment
#'
#'
#' @return the dataframe with GPS location and a cluster column with
#' a number comprised between 1 and K, corresponding to the herd assignment.
#'
#'
#' @export
#' @references


getHerdAssignment <- function(df, K, PlotIt = TRUE){

  dfclust <- with(df, aggregate(list(x=x,y=y),list(ID_Year=ID_Year),mean, na.rm = TRUE))
  input <- dfclust[,c("x","y")]

    clusters <- kmeans(input, centers = K, nstart = 100)
    dfclust$cluster <- clusters$cluster
    if(PlotIt){
    plot(y~x, data = dfclust, main = paste(K, "herds", sep=" "), bty="l", type="n")
    points(y~x, data = dfclust, col = rainbow(K)[dfclust$cluster], pch=3)
    legend("bottomleft", bty="n",  pch=3, col = rainbow(K), legend = 1:K, title = "Herd number") }
    keep <- merge(df, dfclust[,c("ID_Year", "cluster")], by = "ID_Year")
    return(keep)
}
