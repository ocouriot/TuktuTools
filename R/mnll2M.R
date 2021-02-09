#' Function that minimises the likelihood of the nll.post and nll.calf models for a given individual
#'
#'
#' @param df dataframe from get.speed function
#' @param int integer value indicating the minimum number of days between
#'     the beginning of the time series and BP (calf birth),
#'     and BP and the end of the time series.
#'     The main reason for this constraint is that a minimum number of data points are required in
#'     each section of the time-series to be able to estimate model parameters.
#'     We recomanded 9 relocations, thereby 3 days.
#' @param kcons vector of the minimum and maximum time it takes the female to recover normal speed (in days)
#'     to recover her normal movement (in days).
#'
#' @return # returns a list of three objects,
#' which contains the MLE values of all parameters and the mnll, AIC coefficient
#' of the no calf or calf model
#'
#'
#' @export
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785

mnll2M <- function(df, int, kcons){

  speed <- na.omit(df$speed)
  speed[speed == 0] <- 1
  speedmean <- mean(speed)
  speedvar <- var(speed)

  time.mid <- na.omit(df$time.mid)
  dhours <- na.omit(df$dhours)

  # Results for model comparison
  results <- matrix(NA, 1, ncol=6)
  colnames(results) <- c("n", "mnll.nocalf","mnll.calf",
                         "AIC.nocalf", "AIC.calf","Best.Model")

  # BPs and actual date and time and recovery (in days)
  BPs <- data.frame(matrix(NA, 1, ncol=3))
  colnames(BPs) <- c("date.BP1.calf", "BP1.calf", "recovery.calf")

  # Sample size
  results[1,"n"] <- length(speed)

  ##
  # M0: No calf
  # This model assumes that the movement pattern is constant for the whole time series
  # and thus the mean speed is constant.
  # The only parameters estimated are beta and alpha, which is the inverse of the rate.
  # It has a analytical solution and
  # thus we can easily get the minimum negative log likelihood (mnll)
  # negative log-likelihood before parturition (assuming speed = average speed before parturition)
  beta.mean <- speedmean / speedvar
  alpha.mean <- speedmean * beta.mean
  MLL0  <- -sum(dgamma(speed, rate = beta.mean, shape = alpha.mean, log = TRUE))
  results[1,"mnll.nocalf"] <- -MLL0

  ##
  # M1: Calf survived
  BPmax <- max(dhours)/24

  if(BPmax-int<=0){
    results[1,"mnll.calf"] <- NA # maximum log-likelihood of the model
    BPs[["BP1.calf"]] <- NA # mle of BP1 in terms of days

    BPs[["date.BP1.calf"]] <- NA #mle of BP1 in real date and time

    BPs[["recovery"]] <- NA
  } else {
    BP1s  <- (int:(BPmax-int)) # All possible BP
    ll1s <- rep(NA,length(BP1s))
    trytogetbp <- function (bp) {
      fit <- try(nll.calf(df = df, BP = bp, kcons = kcons),silent=TRUE)
      if (!inherits(fit, "try-error"))
        return(fit$Log.Likelihood) else return(NA)}

    ll1s <- sapply(BP1s, trytogetbp)
    ll1s[ll1s %in% c(-Inf, Inf)] <- NA
    MLL1 <- which.max(ll1s)

    if(length(MLL1)==0){
      results[1,"mnll.calf"] <- NA
      BPs[["BP1.calf"]] <- NA
      BPs[["date.BP1.calf"]] <- NA
      BPs[["recovery.calf"]] <- NA
    } else {
      results[1,"mnll.calf"] <- ll1s[[MLL1]] # maximum log-likelihood of the model
      BPs[["BP1.calf"]] <- BP1s[MLL1] # BP1 in terms of days

      BPs[["date.BP1.calf"]] <- time.mid[which.min(abs(dhours-(BPs[["BP1.calf"]]*24)))] # BP1 in real date and time

      BPs[["recovery.calf"]] <- round(nll.calf(df = df, BP = BPs[["BP1.calf"]], kcons = kcons)$par["recovery"]/24)
    }
  }





  # Calculate AIC and compare models
  results[[1,"AIC.nocalf"]] <- 2*(-as.numeric(results[[1,"mnll.nocalf"]]) + 2)
  results[[1,"AIC.calf"]] <- ifelse(is.na(results[[1,"mnll.calf"]]) == FALSE & results[[1,"mnll.calf"]] != 0, 2*(-as.numeric(results[[1,"mnll.calf"]]) + 4), NA)
  results[[1,"Best.Model"]] <- substr(names(which.min(results[,4:5])),5,nchar(names(which.min(results[,4:5]))))


  return(list(results=results, BPs=BPs))

}
