#' The ELPD on both sub-datasets as a function of eta, x1, and x2
#'
#' @param eta learning rate
#' @param x1 sample mean of the first dataset
#' @param x2 sample mean of the second dataset
#' @param const experiment constants
#'
#' @return numeric
#' @export
elpd_joint <- function(x, eta, const) {
  # Unpack constants
  s2z <- const$s2z
  s2y <- const$s2y

  s2_phi <- phi_var(eta, const)
  s2_phi_theta <- phi_theta_var(eta, const)
  S <- matrix(c(s2_phi + s2z, s2_phi, s2_phi, s2_phi_theta + s2y), nrow = 2)

  # Truth
  mu_true <- c(0, 1)
  S_true <- matrix(c(s2z, 0, 0, s2y), nrow = 2)

  elpd <- -log(2*pi) - 0.5*log(det(S)) - 0.5*sum(diag(solve(S) %*% S_true)) - 0.5*t(x) %*% solve(S) %*% x

  return(as.numeric(elpd))
}
