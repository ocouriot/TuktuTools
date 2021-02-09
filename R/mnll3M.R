#' Function that minimises the likelihood of the nll.post, nll.calf and nll.calfdeath models for a given individual
#'
#'
#' @param df dataframe from get.speed function
#' @param int integer value indicating the minimum number of days between
#'     the beginning of the time series and the first BP (calf birth),
#'     the last BP (calf death) and the end of the time series,
#'     and between the two BPs.
#'     The main reason for this constraint is that a minimum number of data points are required in
#'     each section of the time-series to be able to estimate model parameters.
#'     We recomanded 9 relocations, thereby 3 days.
#' @param kcons vector of the minimum and maximum time it takes the female to recover normal speed (in days)
#'     to recover her normal movement (in days).
#'
#' @return # returns a list of three objects,
#' which contains the MLE values of all parameters and the mnll, AIC coefficient
#' of all no calf, calf and calf death models models
#'
#'
#' @export
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785

mnll3M <- function(df, int, kcons){

  speed <- na.omit(df$speed)
  speed[speed == 0] <- 1
  speedmean <- mean(speed)
  speedvar <- var(speed)

  time.mid <- na.omit(df$time.mid)
  dhours <- na.omit(df$dhours)

  # Results for model comparison
  results <- matrix(NA, 1, ncol=8)
  colnames(results) <- c("n", "mnll.nocalf","mnll.calf","mnll.calfdeath",
                         "AIC.nocalf", "AIC.calf", "AIC.calfdeath","Best.Model")

  # BPs and actual date and time and recovery (in days)
  BPs <- data.frame(matrix(NA, 1, ncol=7))
  colnames(BPs) <- c("date.BP1.calf", "date.BP1.calfdeath", "date.BP2.calfdeath",
                     "BP1.calf", "BP1.calfdeath", "BP2.calfdeath","recovery.calf")

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

  ##
  # M2: Calf lost
  # Getting all possible combination of BPs
  # Note that BP are constrained to be int number of non-missing steps apart.
  # To make the code run faster, the BPs are also limited to be less than
  # maximum number of steps it takes for the female to recover her movement apart
  if(BPmax-int <=0){
    results[1,"mnll.calfdeath"] <- NA # mnll2

    BPs[["BP1.calfdeath"]] <- NA #mle of BP1.calfdeath in days
    BPs[["BP2.calfdeath"]] <- NA #mle of BP2.calfdeath in days
    BPs[["date.BP1.calfdeath"]] <- NA  #mle of BP1 in real date and time
    BPs[["date.BP2.calfdeath"]] <- NA  #mle of BP2 in real date and time
  } else {
    BP2s <- expand.grid(list(BP1=int:(BPmax-int),BP2=int:(BPmax-int)))
    BP2s <- BP2s[(BP2s$BP2-BP2s$BP1)>=int,]
    BP2s <- BP2s[(BP2s$BP2-BP2s$BP1) < kcons[2],]

    if(dim(BP2s)[1] == 0)
    {
      results[1,"mnll.calfdeath"] <- NA # mnll2

      BPs[["BP1.calfdeath"]] <- NA # BP1.calfdeath in days
      BPs[["BP2.calfdeath"]] <- NA # BP2.calfdeath in days
      BPs[["date.BP1.calfdeath"]] <- NA  # BP1 in real date and time
      BPs[["date.BP2.calfdeath"]] <- NA  # BP2 in real date and time
    } else{
      trytogetbp2 <- function (bp1,bp2) {
        fit <- try(nll.calfdeath(df = df, BP1 = bp1, BP2 = bp2, kcons = kcons),silent=T)
        if (!inherits(fit, "try-error"))
          return(list(lls = fit$Log.Likelihood, recovery = fit$par[["recovery"]])) else return(list(lls = NA, recovery = NA))}

      ll2s <- rep(NA,length(BP2s$BP1))
      recoveries <- rep(NA, length(BP2s$BP1))
      for(i in 1:length(BP2s$BP1)){
        ll2s[i] <- trytogetbp2(bp1 = BP2s$BP1[i], bp2 = BP2s$BP2[i])$lls
        recoveries[i] <- trytogetbp2(bp1 = BP2s$BP1[i], bp2 = BP2s$BP2[i])$recovery
      }
      BP2s <- cbind(BP2s,ll2s,recoveries)
      BP2s$recoveries <- BP2s$recoveries/24
      BP2s <- BP2s[(BP2s$BP2-BP2s$BP1)<BP2s$recoveries,]
      BP2s$ll2s[BP2s$ll2s %in% c(-Inf, Inf)] <- NA
      MLL2 <- which.max(BP2s$ll2s)

      if(length(MLL2)==0){
        results[1,"mnll.calfdeath"] <- NA
        BPs[["BP1.calfdeath"]] <- NA
        BPs[["BP2.calfdeath"]] <- NA
        BPs[["date.BP1.calfdeath"]] <- NA
        BPs[["date.BP2.calfdeath"]] <- NA
      } else {
        results[1,"mnll.calfdeath"] <- BP2s$ll2s[[MLL2]] # mnll2

        BPs[["BP1.calfdeath"]] <- BP2s[MLL2,"BP1"] # BP1.calfdeath in days
        BPs[["BP2.calfdeath"]] <- BP2s[MLL2,"BP2"] # BP2.calfdeath in days
        BPs[["date.BP1.calfdeath"]] <- time.mid[which.min(abs(dhours-(BPs[["BP1.calfdeath"]]*24)))]  # BP1 in real date and time
        BPs[["date.BP2.calfdeath"]] <- time.mid[which.min(abs(dhours-(BPs[["BP2.calfdeath"]]*24)))]  # BP2 in real date and time
      }
    }
  }



  # Calculate AIC and compare models
  results[[1,"AIC.nocalf"]] <- 2*(-as.numeric(results[[1,"mnll.nocalf"]]) + 2)
  results[[1,"AIC.calf"]] <- ifelse(is.na(results[[1,"mnll.calf"]]) == FALSE & results[[1,"mnll.calf"]] != 0,2*(-as.numeric(results[[1,"mnll.calf"]]) + 5),NA)
  results[[1,"AIC.calfdeath"]] <- ifelse(is.na(results[[1,"mnll.calfdeath"]])== FALSE & results[[1,"mnll.calfdeath"]] != 0, 2*(-as.numeric(results[[1,"mnll.calfdeath"]]) + 6),NA)

  results[[1,"Best.Model"]] <- substr(names(which.min(results[,5:7])),5,nchar(names(which.min(results[,5:7]))))


  return(list(results=results, BPs=BPs))

}
