phi <- function(eta, z, y, const) {
  # Unpack the constants
  n <- const$n
  m <- const$m
  s2z <- const$s2z
  s2y <- const$s2y
  s2t <- const$s2t

  # Calculate lambda
  le <- lambda_eta(const, eta)

  # Calculate and return the posterior mean of phi
  (1 - le)*z + le*y
}
