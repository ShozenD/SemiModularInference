#' Posterior variance of phi + theta
#'
#' @param eta learning rate
#' @param x1 sample mean of the first dataset
#' @param x2 sample mean of the second dataset
#' @param const constants
#'
#' @return
#' @export
phi_theta_var <- function(eta, const) {
  phi_var(eta, const) + theta_var(const)
}
