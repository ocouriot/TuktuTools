#' Likelihood of Female giving birth
#'
#' nll.calf calculates the negative log-likelihood of a female with a calf that survived.
#'
#' This model assumes that the mean speed is constant prior to the birth of a calf.
#'
#' Once the calf is born the speed decrease close to 0.
#' Then the speed increases to reach the same speed as prior to calf birth.
#' The increase depends on the time it takes to recover the normal movement (in days),
#' which is represented by k.
#'
#' Once the mean speed reaches the value from before the birth of the calf,
#' it remains at this value.
#'
#' @param df dataframe from get.speed function
#' @param BP Break point corresponding to calf birth (in days from the begining of the dataset)
#' @param kcons vector of the minimum and maximum time it takes the female to recover normal speed (in days)
#' @param PlotMe if PlotMe = TRUE, a plot of the speed in function to the date and time will be drawn
#' for each individual with the line of the speed estimated by the function with depending of BP
#'
#' @return Returns the negative log-likelihood of this model and the recovery time (in days)
#'
#'
#' @export
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785



nll.calf <- function (df, BP1, kcons, PlotMe = FALSE) {

  # Divides the time series into two sections:
  # a: before the birth of the calf
  # b: after the birth of the calf
  df <- droplevels(subset(df, is.na(speed)==FALSE))
  # speed vector
  speed <- df$speed
  speed.a <- df$speed[df$dhours <= BP1*24]
  speed.b <- df$speed[df$dhours > BP1*24]

  # hour vector (in hours difference between each value and the first one)
  dhours <- df$dhours
  dhours.a <- df$dhours[df$dhours <= BP1*24]
  dhours.b <- df$dhours[df$dhours > BP1*24]
  dhours.b <- dhours.b - dhours.b[1]

  # date time vector
  time.mid.b <- df$time.mid[df$dhours > BP1*24]

  # average speed before parturition
  speedmean.a <- mean(speed.a)

  # speed variance before parturition
  speedvar.a <- var(speed.a)


  ### negative log-likelihood before parturition (assuming speed = average speed before parturition)
  beta.hat <- speedmean.a / speedvar.a
  alpha.hat <- speedmean.a * beta.hat
  nlla <- -sum(dgamma(speed.a, rate = beta.hat, shape = alpha.hat, log = TRUE))


  ### optimize the function nll.post (after parturition)
  par0 <- c(log.beta.calf=0,
            recovery = mean(kcons)*24)
  alpha.calf <- ifelse(alpha.hat>1, 1, alpha.hat)
  mod <- optim(par0, nll.post, alpha.calf = alpha.calf, alpha.mean = alpha.hat, beta.mean = beta.hat, dhours.b = dhours.b, speed.b = speed.b,
               hessian = TRUE, method = "L-BFGS-B",upper=c(log.beta.calf = Inf, recovery=kcons[2]*24),
               lower=c(log.beta.calf = log(beta.hat*2), recovery=kcons[1]*24))
  # control=list(trace=3))

  # get the negative log-likelihood
  nllb <- mod$value
  # get the recovery (in days)
  recovery.hat <- mod$par["recovery"]/24
  # get the estimated parameters
  par <- mod$par
  par <- c(par, alpha.mean = alpha.hat, beta.mean=beta.hat, alpha.calf = alpha.calf)

  ### get the total negative log-likelihood of the calf model (nll before parturition and nll after parturition)
  total.nll = nlla + nllb


  ### function to calculate the fitted values

  fitted.calf <- function(par, dhours.a, dhours.b){
    alpha.mean <- par["alpha.mean"]
    beta.mean <-  par["beta.mean"]
    alpha.calf <- par["alpha.calf"]
    beta.calf <-  exp(par["log.beta.calf"])
    recovery <- par["recovery"]

    # speed before birth
    speed.hat.a <- rep(alpha.mean/beta.mean,length(dhours.a))

    # calculation of speed after birth
    alpha.slope.b <- (alpha.mean - alpha.calf) / recovery
    beta.slope.b <- (beta.mean - beta.calf) / recovery
    alpha.hat.b <- alpha.calf + alpha.slope.b * dhours.b
    alpha.hat.b[dhours.b >  recovery] <- alpha.mean
    beta.hat.b <- beta.calf + beta.slope.b * dhours.b
    beta.hat.b[dhours.b > recovery] <- beta.mean

    speed.hat.b <- alpha.hat.b / beta.hat.b

    speed.hat <- c(speed.hat.a, speed.hat.b)

    return(fit = speed.hat)
  }

  fit <- fitted.calf(par = par, dhours.a = dhours.a, dhours.b = dhours.b)

  ### function to plot the speed in function to hours with fitted values
  plotFit <- function(ID, dhours, speed, time.mid, time.mid.b, alpha.mean, beta.mean, speed.hat, recovery){
    plot(time.mid, speed, type = "l", main = paste("Individual", unique(ID), sep = " ", bty = "l"))
    lines(time.mid, speed.hat, col = 2, lwd = 2)
    text(time.mid[which(time.mid==time.mid.b[1])-1]+2*3600*24,alpha.mean/beta.mean+600,
         labels = paste("Calving = ", substr(time.mid[which(time.mid==time.mid.b[1])-1],1,10),sep=""),srt=90,font=3, col="red")
    text(mean(time.mid),max(speed),
         labels = paste(paste("recovery = ", round(recovery/24,0),sep=""), "days", sep = " "),font=3)
  }
  if (PlotMe)
    plotFit(ID = df$ID, speed = speed, dhours = dhours, alpha.mean = par["alpha.mean"], recovery = par["recovery"],
            time.mid = df$time.mid, time.mid.b = time.mid.b, beta.mean = par["beta.mean"], speed.hat = fit)


  return(list(Log.Likelihood = -total.nll, par=par, fit = fit))
}
