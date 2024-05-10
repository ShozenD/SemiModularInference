#' Plot the envelope of the ELPD for the first dataset
#'
#' @param xmin the minimum value of the x-axis
#' @param xmax the maximum value of the x-axis
#' @param x1 the sample mean of the first dataset
#' @param x2 the sample mean of the second dataset
#'
#' @return ggplot object
#' @export
plot_elpd_phi <- function(xmin, xmax, x1, x2) {
  x <- seq(xmin, xmax, length.out = 200)
  log_eta <- seq(log10(0.0005), 0, length.out = 200)
  df <- expand.grid(x = x, log_eta = log_eta)
  df <- dplyr::mutate(df, y = elpd_phi(x, 10**(log_eta), const))

  x.2 <- phi_mean(10**(log_eta), x1, x2, const)
  elpd <- elpd_phi(x.2, 10**(log_eta), const)
  df_interp <- data.frame(x2 = x.2, y2 = elpd)

  ggplot(df, aes(x, y)) +
    geom_rect(aes(xmin = min(x.2), xmax = max(x.2), ymax = Inf, ymin = -Inf), fill = "#f1f1f1", alpha = 0.3) +
    geom_line(aes(color = log_eta, group = log_eta), alpha = 0.7) +
    scale_color_gradient2(midpoint = median(log_eta),low = "#004c6d", mid = "#f1f1f1", high = "#de425b") +
    geom_line(data = df_interp, aes(x2, y2), color = "#ffa600", linewidth = 1) +
    geom_point(x = x.2[which.max(elpd)], y = max(elpd),
               color = "#ffa600",
               fill = "white",
               size = 2, shape = 21) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    scale_x_continuous(expand = c(0, 0), limits = c(xmin, xmax)) +
    labs(y = "ELPD", x = expression(varphi),
         color = expression(log[10](eta)),
         subtitle = bquote(paste(bar(x)[1], " = ", .(round(x1, 4)), ", ", bar(x)[2], " = ", .(round(x2, 4)))))
}
