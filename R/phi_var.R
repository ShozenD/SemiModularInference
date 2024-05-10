#' The posterior variance of phi
#'
#' @param eta learning rate
#' @param const constants
#'
#' @return
#' @export
phi_var <- function(eta, const) {
  # Unpack the data
  n <- const$n
  m <- const$m
  s2z <- const$s2z
  s2y <- const$s2y
  s2t <- const$s2t

  # Calculate and return the posterior variance of phi
  (s2z/n) / (s2y/m + eta*(s2z/n + s2t)) * (s2y/m + eta*s2t)
}
