#' Evaluates the ELPD of phi + theta given eta, x1, and x2
#'
#' @param eta learning rate
#' @param x1 sample mean of the first dataset
#' @param x2 sample mean of the second dataset
#' @param const experiment constants
#'
#' @return
#' @export
elpd_phi_theta_2 <- function(eta, x1, x2, const) {
  phi_theta <- phi_theta_mean(eta, x1, x2, const)
  elpd_phi_theta(phi_theta, eta, const)
}
