#' Return the value of lambda for a specific eta
#'
#' @param eta learning rate
#' @param const a list of contants
#'
#' @return a vector
#' @export
lambda_eta <- function(eta, const) {
  # Unpack the data
  n <- const$n
  m <- const$m
  s2z <- const$s2z
  s2y <- const$s2y
  s2t <- const$s2t

  # Calculate and return lambda
  (eta*s2z/n) / (s2y/m + eta*(s2z/n + s2t))
}
