#' Posterior mean of phi + theta
#'
#' @param eta learning rate
#' @param x1 sample mean of the first dataset
#' @param x2 sample mean of the second dataset
#' @param const constants
#'
#' @return
#' @export
phi_theta_mean <- function(eta, x1, x2, const) {
  phi_mean(eta, x1, x2, const) + theta_mean(eta, x1, x2, const)
}
