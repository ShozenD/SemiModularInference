#' The ELPD as a function of phi and eta
#'
#' @param x the value of phi
#' @param eta learning rate
#' @param const constants
#'
#' @return numeric
#' @export
elpd_phi <- function(x, eta, const) {
  s2z <- const$s2z
  v <- phi_var(eta, const)

  -0.5*(log(s2z + v) + s2z/(v + s2z) + 1/(v + s2z)*x^2)
}
