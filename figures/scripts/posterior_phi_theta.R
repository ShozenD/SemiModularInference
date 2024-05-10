# Load libraries
library(ggplot2)
library(patchwork)
library(devtools)
load_all()

# ===== Setup =====
# Fixed parameters
n <- 25; m <- 50;
phi_true <- 0; theta_true <- 1;
s2z <- 4; s2y <- 1;

# Prior
s2t <- 0.5

const <- list(n = n,
              m = m,
              s2z = s2z,
              s2y = s2y,
              s2t = s2t)
# =====

zbar <- -0.11; ybar <- 0.87;
eta <- seq(0, 1, length.out = 100)
df_theta <- data.frame(
  eta = eta,
  M = theta_mean(eta, zbar, ybar, const),
  V = theta_var(const)
)

plt_theta <- ggplot(df_theta, aes(eta, M)) +
  geom_line(color = "#ffa600") +
  geom_hline(yintercept = theta_true, linetype = "dashed") +
  geom_ribbon(aes(ymin = M - sqrt(V), ymax = M + sqrt(V)),
              fill = "#ffa600",
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = expression(eta),
       y = bquote(paste(theta, "|", hat(varphi)[eta])),
       subtitle = bquote(paste(bar(x)[1], " = ", .(round(zbar, 2)), ", ", bar(x)[2], " = ", .(round(ybar, 2)))))

df_phi_plus_theta <- data.frame(
  eta = eta,
  M = phi_theta_mean(eta, zbar, ybar, const),
  V = phi_theta_var(eta, const)
)

plt_phi_theta <- ggplot(df_phi_plus_theta, aes(eta, M)) +
  geom_line(color = "#ffa600") +
  geom_hline(yintercept = phi_true + theta_true, linetype = "dashed") +
  geom_ribbon(aes(ymin = M - sqrt(V), ymax = M + sqrt(V)),
              fill = "#ffa600",
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = expression(eta),
       y = bquote(paste(hat(varphi)[eta], "+", theta, "|", hat(varphi)[eta])),
       subtitle = bquote(paste(bar(x)[1], " = ", .(round(zbar, 2)), ", ", bar(x)[2], " = ", .(round(ybar, 2)))))

plt_theta + plt_phi_theta + plot_layout(guides = 'collect', axes = "collect") &
  theme(
    plot.subtitle = element_text(size = 9),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.width = unit(0.5, "cm"),
    legend.margin = margin(-0.2, 0, 0, 0, "cm"),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

ggsave("figures/posterior_phi_theta.png", width = 12, height = 5, dpi = 300, units = "cm")
