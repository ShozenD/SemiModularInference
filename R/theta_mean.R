#' Conditional posterior mean of theta given phi
#'
#' @param eta learning rate
#' @param x1 the sample mean of the first dataset
#' @param x2 the sample mean of the second dataset
#' @param const constants
#'
#' @return
#' @export
theta_mean <- function(eta, x1, x2, const) {
  m <- const$m
  s2y <- const$s2y
  s2t <- const$s2t

  rho <- s2y*s2t/(s2y/m + s2t)
  rho*(1 - lambda_eta(eta, const))*(x2 - x1)
}
