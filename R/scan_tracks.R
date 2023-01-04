#' Scan Multiple Movement Tracks at a Glance
#'
#' Takes list of individual (simple feature) tracks, and plots their tracks.
#' Useful, e.g., for comparing proximity of animals. Includes a feature which computes
#' and plots the distance between daily mean locations.
#'
#' @param x data frame, simple feature, or list of simple feature tracks
#' @param id.col name ID column
#' @param time.col name of POSIX time column
#' @param colors vector of colors
#' @param plotdistance whether to plot pairwise distances (only works for list of length 2) [COULD BE TRUNCATED]
#' @param distance.df distance data frame (output of \code{\link{getPairwiseDistances}}) [COULD BE TRUNCATED]
#' @param d.col color for distances [COULD BE TRUNCATED]
#' @param legend whether or not to add a legend
#' @param legend.pos location of legend
#' @param ... additional parameters to pass to legend function
#' @example examples/example_scan_tracks.R
#' @return If plotdistance = TRUE, returns the pairwise distance data frame.
#' @export


scan_tracks <- function(x, 
                        id.col  = "ID",
                        time.col = "Time",
                        colors = NULL,
                        distance.df = NULL,
                        plotdistance = FALSE,
                        d.col = "darkgrey",
                        legend = FALSE,
                        threshold = 1e3, 
                        legend.pos = "bottomleft", 
                        ...){

  par.init <- par(no.readonly = TRUE)
  on.exit(par(par.init))
  
  if(is(x)[[1]] == "list") {
    sf.list <- x
    ids <- sapply(sf.list, function(sf) data.frame(sf)[1,id.col]) %>% droplevels
    names(sf.list) <- ids
    xy.df <- ldply(sf.list,
                   function(sf) cbind(as.data.frame(st_coordinates(sf)),
                                      Time= data.frame(sf)[,time.col])) 
    names(attr(sf.list,"split_labels")) <- "ID"
    xy.df <- plyr::arrange(xy.df, ID, Time) 
  } else if(inherits(x, "sf")){
    xy.df <- cbind(ID = data.frame(x)[,id.col],
                   Time= data.frame(x)[,time.col],
                   as.data.frame(st_coordinates(x)))
    sf.list <- dlply(xy.df, "ID", st_as_sf, coords = c("X","Y"), crs = st_crs(x))
  } else if(is(x)[[1]] == "data.frame"){
    xy.df <-  mutate(x, Time = get(time.col), ID = get(id.col))
    if(time.col != "Time") xy.df[,time.col] <- NULL
    if(id.col != "ID") xy.df[,id.col] <- NULL
  }
  xy.df$ID <- factor(xy.df$ID)
  xy.df <- droplevels(xy.df)
  ids <- levels(xy.df$ID)
  if(is.null(colors)) colors <- 1:length(ids)
  xy.df$color = colors[match(xy.df$ID, ids)]

  xlim <- range(xy.df$X)
  ylim <- range(xy.df$Y)
  tlim <- range(xy.df$Time)
  
  if(plotdistance){
    if(is.null(distance.df))
      distance.df <- getPairwiseDistances(sf.list)
    layout(rbind(1:2, c(1,3), c(1,4)))
    } else layout(rbind(1:2, c(1,3)))

  par(mar = c(0,4,0,0), oma = c(4,0,4,4), xpd = NA, bty = "l",
      mgp = c(1.5,.5,0), tck = 0.01, cex.lab = 1, cex.axis = 0.8)

  plot(0,0,xlim = xlim, ylim = ylim, asp =1, type = "n", xlab = "X", ylab = "Y")
  d_ply(xy.df, "ID", function(xy) lines(xy$X, xy$Y, col = xy$col[1]))
  if(legend) legend(legend.pos, 
                    col = colors, 
                    legend = levels(xy.df$ID), lty = 1, ...)

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
  par(par.init)
  invisible(distance.df)
}
