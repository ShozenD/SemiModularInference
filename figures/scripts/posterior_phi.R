# Load libraries
library(ggplot2)
library(patchwork)

# ========== experiment set-up ==========
# Fixed parameters
n <- 25; m <- 50;
phi_true <- 0; theta_true <- 1;
s2z <- 4; s2y <- 1;

# Prior
s2t <- 0.3

# Collect constants
const <- list(n = n,
              m = m,
              s2z = s2z,
              s2y = s2y,
              s2t = s2t)
# =======================================

zbar <- -0.1131528
ybar <- 0.8716570

eta <- seq(0, 1, length.out = 100)
df_phi <- data.frame(eta = eta,
                     M = phi_mean(eta, zbar, ybar, const),
                     V = phi_var(eta, const),
                     SD = sqrt(phi_var(eta, const)))

plt_phi_mean <- ggplot(df_phi, aes(eta, M)) +
  geom_line(color = "#003f5c") +
  geom_hline(yintercept = phi_true, linetype = "dashed") +
  scale_x_continuous(expand = c(0,0)) +
  labs(
    x = expression(eta),
    y = expression(hat(varphi)[eta])
  )

plt_phi_var <- ggplot(df_phi, aes(eta, V)) +
  geom_line(color = "#bc5090") +
  scale_x_continuous(expand = c(0,0)) +
  labs(
    x = expression(eta),
    y = expression(hat(sigma)[varphi][eta]^2)
  )

plt_phi_sd <- ggplot(df_phi, aes(eta, M)) +
  geom_line(color = "#ffa600") +
  geom_hline(yintercept = phi_true, linetype = "dashed") +
  scale_x_continuous(expand = c(0,0)) +
  geom_ribbon(aes(ymin = M - SD, ymax = M + SD),
              fill = "#ffa600",
              alpha = 0.3) +
  labs(
    x = expression(eta),
    y = expression(hat(varphi)[eta])
  )

plt_phi_mean + plt_phi_var + plt_phi_sd +
  plot_annotation(
    subtitle = bquote(paste(bar(x)[1],
                            " = ",
                            .(round(zbar, 2)),
                            ", ",
                            bar(x)[2],
                            " = ",
                            .(round(ybar, 2))))
  ) &
  theme(
    plot.subtitle = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

# Save plot
ggsave("figures/posterior_phi.png", width = 16, height = 5, dpi = 300, units = "cm")
