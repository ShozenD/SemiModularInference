# Load libraries
library(ggplot2)
library(patchwork)
library(devtools)
load_all()

# Set ggplot theme
theme_set(theme_bw())

# ========== experiment set-up ==========
# Fixed parameters
n <- 25; m <- 50;
phi_true <- 0; theta_true <- 1;
s2z <- 4; s2y <- 1;

# Prior
s2t <- 0.5

# Collect constants
const <- list(n = n,
              m = m,
              s2z = s2z,
              s2y = s2y,
              s2t = s2t)
# =======================================

zbar <- 0.275; ybar <- 0.52;
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_1 <- plot_elpd_phi_theta(phi_theta_0 - 0.001, phi_theta_1 + 0.001, zbar, ybar)

zbar <- 0.59; ybar <- 0.6045;
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_2 <- plot_elpd_phi_theta(phi_theta_0 - 0.0001, phi_theta_1 + 0.0001, zbar, ybar)

zbar <- 0.59; ybar <- 0.608;
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_3 <- plot_elpd_phi_theta(phi_theta_0 - 0.0001, phi_theta_1 + 0.0001, zbar, ybar)

zbar <- 0.15; ybar <- 1.15
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_4 <- plot_elpd_phi_theta(phi_theta_0 - 0.01, phi_theta_1 + 0.01, zbar, ybar)

zbar <- 1.38; ybar <- 1.39
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_5 <- plot_elpd_phi_theta(phi_theta_0 - 0.0001, phi_theta_1 + 0.0001, zbar, ybar)

zbar <- 1.39; ybar <- 1.4
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_6 <- plot_elpd_phi_theta(phi_theta_0 - 0.0001, phi_theta_1 + 0.0001, zbar, ybar)

# Gather plots
(plt_1 + plt_2 + plt_3) / (plt_4 + plt_5 + plt_6) +
  plot_layout(guides = 'collect', axes = "collect") &
  theme(
    plot.subtitle = element_text(size = 8),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.width = unit(0.5, "cm"),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
  )

# Export figure
ggsave("figures/optim_elpd_phi_theta_edge_cases.png", width = 16, height = 10, dpi = 300, units = "cm")
