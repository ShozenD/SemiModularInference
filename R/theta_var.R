#' Conditional variance of theta given phi
#'
#' @param const constants
#'
#' @return
#' @export
theta_var <- function(const) {
  m <- const$m
  s2y <- const$s2y
  s2t <- const$s2t
  rho <- s2y*s2t/(s2y/m + s2t)

  rho/m
}
