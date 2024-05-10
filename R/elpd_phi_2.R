#' Evaluates the ELPD of phi given eta, x1, and x2
#'
#' @param eta the learning rate
#' @param x1 the sample mean of the first dataset
#' @param x2 the sample mean of the second dataset
#' @param const experiment constant
#'
#' @return numeric
#' @export
elpd_phi_2 <- function(eta, x1, x2, const) {
  phi <- phi_mean(eta, x1, x2, const)
  elpd_phi(phi, eta, const)
}
