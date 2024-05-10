#' Calculate the ELPD of phi + theta
#'
#' @param x the value of phi + theta
#' @param eta learning rate
#' @param const constants
#'
#' @return ELPD value
#' @export
elpd_phi_theta <- function(x, eta, const) {
  s2y <- const$s2y
  v <- phi_theta_var(eta, const) + s2y

  -0.5*(log(2*pi) + log(v) + s2y/v + (x - 1)**2/v)
}
