#' Scan Multiple Tracks at a Glance
#'
#' Takes list of individual (simple feature) tracks, and plots their tracks.
#' Useful, e.g., for comparing proximity of animals. Includes a feature which computes
#' and plots the distance between daily mean locations.
#'
#' @param sf.list list of simple feature tracks
#' @param cols vector of colors
#' @param plotdistance whether to plot pairwise distances (only works for list of length 2)
#' @param distance.df distance data frame (output of \code{\link{getPairwiseDistances}})
#' @param d.col col of distances
#' @param legend whether or not to add a legend
#'
#' @return If plotdistance = TRUE, returns the pairwise distance data frame.
#'
#' @examples
#' data(bathurst)
#' b.subset <- bathurst %>% subset(Year == 2014)
#' b.list <- dlply(b.subset, "ID", st_as_sf)
#'
#'
#' # basic quicky implementation
#' require(gplots)
#' palette(rich.colors(length(b.list)))
#' scan_tracks(b.list)
#'
#' # with pairwise distances of a subset of indviduals
#' b.distances <- scan_tracks(b.list[1:6], cols = rich.colors(6), legend = TRUE,
#'             plotdistance = TRUE, threshold = 1e3)
#'
#' head(b.distances)

scan_tracks <- function(sf.list, cols = 1:length(sf.list),
                        distance.df = NULL,
                        plotdistance = FALSE,
                        d.col = "darkgrey",
                        legend = FALSE,
                        threshold = 1e3, ...){

  par.init <- par(no.readonly = TRUE)
  on.exit(par(par.init))
  ids <- sapply(sf.list, function(sf) sf$ID[[1]]) %>% droplevels
  names(sf.list) <- ids
  xy.df <- ldply(sf.list,
                 function(sf) cbind(as.data.frame(st_coordinates(sf)),
                                    Time= sf$Time)) %>%
    plyr::rename(c(.id = "ID")) %>%
    mutate(col = cols[match(ID, ids)])

  xlim <- range(xy.df$X)
  ylim <- range(xy.df$Y)
  tlim <- range(xy.df$Time)


  if(plotdistance){
    if(is.null(distance.df))
      distance.df <- getPairwiseDistances(sf.list)
    layout(rbind(1:2, c(1,3), c(1,4)))
    } else layout(rbind(1:2, c(1,3)))

  par(mar = c(0,4,0,0), oma = c(4,0,4,4), xpd = NA, bty = "l",
      mgp = c(1.5,.5,0), tck = 0.01, cex.lab = 1.2)

  plot(0,0,xlim = xlim, ylim = ylim, asp =1, type = "n", xlab = "X", ylab = "Y")
  d_ply(xy.df, "ID", function(xy) lines(xy$X, xy$Y, col = xy$col[1]))
  if(legend) legend("topleft", cex = 0.75, col = cols, legend = paste(unique(xy.df$ID), unique(year(xy.df$Time)), sep =" - "), lty = 1)

  if(plotdistance){
    with(distance.df,
         plot(0, 0, ylim = c(0, max(distance.df$distance)),
              xlim = tlim, ylab = "distance (m)", type = "n",
              xlab = "", xaxt="n"))
     d_ply(distance.df, "pair",
           function(df) lines(df[,c("Date","distance")], col = d.col))
  }

  plot(xy.df[,"Time"], xy.df[,"X"], xlim = tlim, ylim = xlim, type = "n", ylab = "X", xaxt = "n", xlab = "")
  d_ply(xy.df, "ID", function(xy) lines(xy$Time, xy$X, col = xy$col[1]))

  plot(xy.df[,c("Time", "Y")],xlim = tlim, ylim = ylim, type = "n", ylab = "Y", xlab = "time")
  d_ply(xy.df, "ID", function(xy) lines(xy$Time, xy$Y, col = xy$col[1]))

  invisible(distance.df)
}
