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
#' data(barrenground)
#' 
#' #new version gets rid of lines where data are missing, looks pretty good for long durations
#' b.list <- dlply(barrenground, "ID", st_as_sf, coords = c("Lon", "Lat"))
#' scan_tracks(b.list)
#' scan_tracks2(b.list) 
#' 
#' #new version gets rid of lines where data are missing, looks patchy at shorter durations
#' b.subset <- barrenground %>% subset(Year == 2017)
#' sub.list <- dlply(b.subset, "ID", st_as_sf, coords = c("Lon", "Lat"))
#' scan_tracks(sub.list)
#' scan_tracks2(sub.list)

scan_tracks2 <- function(sf.list, cols = 1:length(sf.list),
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
                                    Time= sf$DateTime)) %>% droplevels %>%
    tidyr::complete(ID, Time) %>% 
    arrange(ID, Time) %>%
    mutate(col = cols[match(ID, ids)])

  xlim <- range(xy.df$X, na.rm = T)
  ylim <- range(xy.df$Y, na.rm = T)
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
  if(legend) legend("topleft", cex = 0.75, col = cols, legend = unique(xy.df$ID), lty = 1)

  if(plotdistance){
    with(distance.df,
         plot(0, 0, ylim = c(0, max(distance.df$distance)),
              xlim = tlim, ylab = "distance (m)", type = "n",
              xlab = "", xaxt="n"))
     d_ply(distance.df, "pair",
           function(df) lines(df[,c("Date","distance")], col = d.col))
  }

  plot(xy.df[,c("Time","X")], xlim = tlim, ylim = xlim, type = "n", ylab = "X", xaxt = "n", xlab = "")
  d_ply(xy.df, "ID", function(xy) lines(xy$Time, xy$X, col = xy$col[1]))

  plot(xy.df[,c("Time", "Y")],xlim = tlim, ylim = ylim, type = "n", ylab = "Y", xlab = "time")
  d_ply(xy.df, "ID", function(xy) lines(xy$Time, xy$Y, col = xy$col[1]))

  invisible(distance.df)
}
