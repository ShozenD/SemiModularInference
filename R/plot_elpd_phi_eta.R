#' Plot the ELPD as a function of eta
#'
#' @param x1 the sample mean of the first dataset
#' @param x2 the sample mean of the second dataset
#'
#' @return ggplot object
#' @export
plot_elpd_phi_eta <- function(x1, x2) {
  log_eta <- seq(log10(0.0005), 0, length.out = 200)
  elpd <- elpd_phi_2(10**log_eta, x1, x2, const)
  df <- data.frame(eta = 10**log_eta, elpd = elpd)

  ggplot(df, aes(eta, elpd)) +
    geom_line(color = "#ffa600", linewidth = 1) +
    geom_point(x = 10**log_eta[which.max(elpd)], y = max(elpd),
               color = "#ffa600", fill = "white",
               size = 2, shape = 21) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0,1,0.1)) +
    labs(y = "ELPD", x = expression(eta),
         subtitle = bquote(paste(bar(x)[1], " = ", .(round(x1, 4)), ", ", bar(x)[2], " = ", .(round(x2, 4)))))
}
