#' Posterior phi given eta, x1, and x2
#'
#' @param eta learning rate
#' @param x1 sample average for the first dataset
#' @param x2 sample average for the second dataset
#' @param const constants
#'
#' @return posterior phi
#' @export
phi_mean <- function(eta, x1, x2, const) {
  # Unpack the constants
  n <- const$n
  m <- const$m
  s2z <- const$s2z
  s2y <- const$s2y
  s2t <- const$s2t

  # Calculate lambda
  le <- lambda_eta(eta, const)

  # Calculate and return the posterior mean of phi
  (1 - le)*x1 + le*x2
}
