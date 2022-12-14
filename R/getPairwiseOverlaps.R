#' Compute pairwise overlaps
#'
#' This function (rather quickly) computes pairwise overlaps between daily mean locations
#' of animals.  By default it uses the "volume intersection" index (see \code{\link{adehabitatHR::kerneloverlap}}).
#'
#' @param sf.list list of simple feature tracks
#' @param column column by which to split the data
#' @param method method of kernel overlap
#' @param ... additional parameters for \code{\link{kerneloverlap}} function - notably "grid"
#' @return data frame with ID1, ID2 and overlap index.
#'

getPairwiseOverlaps <- function(sf.list, column = "ID", method = "VI", ...){
  sp.list <- lapply(sf.list, sf::as_Spatial)
  sp.df <- do.call(rbind, sp.list)
  col <- eval(parse(text = paste0("sp.df$", column)))
  eval(parse(text = paste0("sp.df$", column, "<- as.character(col)")))

  o1 <- adehabitatHR::kerneloverlap(sp.df[,column], method = method, ...)
  n <- nrow(o1)

  ID1.matrix <- matrix(rep(row.names(o1), n), ncol = n)
  ID2.matrix <- t(ID1.matrix)
  ut <- upper.tri(ID1.matrix)

  data.frame(ID1 = ID1.matrix[ut], ID2 = ID2.matrix[ut], overlap = o1[ut])
}


