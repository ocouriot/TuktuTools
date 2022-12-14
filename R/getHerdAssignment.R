#' Get herd assignment using spatial clustering
#'
#' Function to identify different herds/populations using K-means analysis on GPS locations of individuals,
#' using the average location for each individual and year.
#'
#' @param df a dataframe containing x and y coordinates (in a metric system) with Time and the ID of individuals 
#' @param K number of herds (can be determined using getKnumber function in this package)
#' @param plotIt if TRUE, plot the GPS location with herd assignment
#'
#'
#' @return the dataframe with GPS location and a cluster column with
#' a number comprised between 1 and K, corresponding to the herd assignment.
#' 
#' @example examples/example_getHerdAssignment.R
#'
#' @references
#' 
#' @export


getHerdAssignment <- function(df, K, PlotIt = TRUE){
  dfclust <- df %>% group_by(ID, Year) %>% summarize(X = mean(x, na.rm = TRUE), Y = mean(y, na.rm=TRUE)) %>% ungroup %>% as.data.frame
    # with(df, aggregate(list(X=x,Y=y),list(ID = ID, Year = Year),mean, na.rm = TRUE))
  input <- dfclust[,c("X","Y")]

    clusters <- kmeans(input, centers = K, nstart = 10)#, algorithm = "Lloyd")
    dfclust$cluster <- clusters$cluster
    if(PlotIt){
    plot(Y~X, data = dfclust, main = paste(paste(K, "herds", sep=" "), paste("day", unique(dfclust$yday), sep = " "), sep = " : "), bty="l", type="n")
    points(Y~X, data = dfclust, col = rainbow(K)[dfclust$cluster], pch=3)
    legend("bottomleft", bty="n",  pch=3, col = rainbow(K), legend = 1:K, title = "Herd number") }
    keep <- merge(df, dfclust[,c("ID", "Year", "cluster")], by = c("ID", "Year"))
    return(keep)
}
