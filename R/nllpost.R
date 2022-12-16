#' Likelihood of Female with a calf
#'
#' nll.post calculates the negative log-likelihood of the section of the time series
#' when the female is with a calf in both cases where the calf survived and the one where the calf died.
#' It is used in part to estimate k.
#' k represents the time it takes, in days, for the female to recover her normal movement.
#'
#' nll.post is minimised within both models where the calf survived and the calf died (nll.calf and nll.calfdeath).
#'
#' The function assumes an increase of the speed of the female's movement after the birth of a calf.


#' @param alpha.mean shape of the Gamma distribution function used to calculate speed before birth
#' @param beta.mean rate of the Gamma distribution function used to calculate speed before birth
#' @param alpha.calf parameter used to calculate the shape of the Gamma distribution function used to calculate speed during recovery
#' @param beta.calf parameter used to calculate the rate of the Gamma distribution function used to calculate speed during recovery
#' @param recovery the recovery time is a possibility
#' @param dhours.b vector of the difference in hour between each speed value and the first one
#' @param speed.b vector of the speed after birth

#' @return Returns the negative log-likelihood of this model given the data SL
#'
#' @references DeMars, C., M. Auger-Méthé, U. Schlägel, S. Boutin, (Published online) Inferring Parturition and Neonate Survival from Movement Patterns of Female Ungulates. Ecology and Evolution. DOI: 10.1002/ece3.785




# function that calculate the negative likelihood after parturition
nllPost <- function(par = c(log.beta.calf = NA, recovery = NA), alpha.mean, beta.mean, alpha.calf, dhours.b, speed.b){

  beta.calf <-  exp(par["log.beta.calf"])
  recovery <- par["recovery"]

  alpha.slope <- (alpha.mean - alpha.calf) / recovery

  beta.slope <- (beta.mean - beta.calf) / recovery

  alpha.hat <- alpha.calf + alpha.slope * dhours.b
  alpha.hat[dhours.b >  recovery] <- alpha.mean

  beta.hat <- beta.calf + beta.slope * dhours.b
  beta.hat[dhours.b > recovery] <- beta.mean

  -sum(dgamma(speed.b, rate = beta.hat, shape = alpha.hat, log = TRUE))
}

