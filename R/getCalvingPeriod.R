#' Bivariate segmentation of the daily movement rate and daily ranging area
#'
#' Function to identify the annual calving period at the population level.
#' The function segmentates a joint time series of daily herd movement rate and ranging area.
#' The function uses a two-dimensional time-space clustering method on the bivariate time series 
#' implemented in the segclust2d R package (Patin et al., 2020). 
#' The segmentation is performed on the square root of the area, which has a more normal distribution 
#' compared to raw area. The segmentation considers 5 days as the minimum number of days per period, 
#' on 3 different periods: pre-calving, calving, post-recovery.

#' @param df a data frame containing columns: the daily ranging area (area), movement rate (speed), day of year (yday) and date (Date)
#' @param PlotIt to visualize the segmentation
#' 
#' @return a dataframe with the dates of the calving period for the year
#'
#' @export
#' 
#' @references Patin, R., Etienne, M., Lebarbier, E., Chamaillé‐Jammes, S., & Benhamou, S. (2020). 
#' Identifying stationary phases in multivariate time series for highlighting behavioural modes and 
#' home range settlements. Journal of Animal Ecology, 89(1), 44–56. https://doi.org/10.1111/1365-2656.13105

getCalvingPeriod <- function(df, PlotIt = TRUE){
  daily_area_movement_clustered <- df %>% 
    mutate(sqrtarea = sqrt(area)) %>% segclust2d::segclust(Kmax = 3, lmin = 5, ncluster = 3, #type = "behavior",
                                                           scale.variable = FALSE, seg.var = c("speed","sqrtarea"))
  
  # M4_states <- ldply(daily_area_movement_clustered, states)
  daily_area_movement_withstate <- segclust2d::augment(daily_area_movement_clustered) %>% 
    mutate(state = (1:3)[match(state, unique(state))])
  
  calving_season <- daily_area_movement_withstate[,c("yday","Date","state")] %>% 
    subset(state == 2) %>% mutate(start = min(yday), end = max(yday), 
                                  date.start= min(Date), 
                                  date.end = max(Date))
  
  if(PlotIt){
    par(mar = c(0,4,0,0), oma = c(4,0,4,2), tck = 0.01, xpd = NA, 
        mgp = c(1.5,.25,0), cex.lab = 1.25, bty = "l")
    print(plotWithStates(daily_area_movement_withstate %>% mutate(y = sqrt(area)),
                         cols = c("blue","tomato","forestgreen"), 
                         v2.lab = expression(sqrt(area)),
                         v1.lab = "daily displacement (m)"))
  }
  
  return(calving_season)
}

plotWithStates <- function(df, cols = 1:3, v1.lab = "x", v2.lab = "y", 
                           main = df$Year[1]){
  layout(rbind(1:2,c(1,3)))
  with(df,{ 
    plot(speed, y, type = "o", col = "grey", xlab = v1.lab, ylab = v2.lab,
         ylim = c(-10,550), xlim = c(-10,1400))
    points(speed, y, col = cols[state], pch = 19)
    mtext(main, side = 3, at = min(speed), cex = 1.5, line = .5)
  })
  
  require(mixtools)
  ddply(df, "state", function(mydf){
    mye <- with(mydf[1,], 
                mixtools::ellipse(mu = c(mu.speed, mu.sqrtarea), 
                                  sigma = diag(c(sd.speed, sd.sqrtarea)^2), 
                                  draw = FALSE))
    polygon(mye[,1], mye[,2], col = cols[mydf$state[1]]  %>% alpha(.2), 
            border = NA)
  })
  
  with(df, {
    plot(yday, y, type = "o", col = "grey", xlab = "", 
         ylab = v2.lab, xaxt ="n",ylim = c(0,500))
    points(yday, y, col = cols[state], pch = 19)}
  )
  
  ddply(df, "state", function(mydf){
    n <- nrow(mydf)
    with(mydf,{
      polygon(c(yday, yday[n:1]), 
              c(mu.sqrtarea-sd.sqrtarea, mu.sqrtarea+sd.sqrtarea), 
              col = alpha(cols[state], .2), bor = NA)
      lines(yday, mu.sqrtarea, col = cols[state], lwd = 2)
    })
  })
  
  with(df, {
    plot(yday, speed, type = "o", col = "grey", 
         xlab = "day of year", ylab = v1.lab, ylim = c(0,1300))
    points(yday, speed, col = cols[state], pch = 19)
  })
  
  ddply(df, "state", function(mydf){
    n <- nrow(mydf)
    with(mydf,{
      polygon(c(yday, yday[n:1]), 
              c(mu.speed-sd.speed, mu.speed+sd.speed), 
              col = alpha(cols[state], .2), bor = NA)
      lines(yday, mu.speed, col = cols[state], lwd = 2)
    })
  })
}





