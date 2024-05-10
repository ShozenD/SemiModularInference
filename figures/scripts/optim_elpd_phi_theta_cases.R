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

# Case 1
zbar <- 0; ybar <- 1.3
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_case_1 <- plot_elpd_phi_theta(phi_theta_0 - 0.03, phi_theta_1 + 0.03, zbar, ybar)

# Case 2
zbar <- -0.5; ybar <- 1.05
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_case_2 <- plot_elpd_phi_theta(phi_theta_0 - 0.1, phi_theta_1 + 0.1, zbar, ybar)

# Case 3
zbar <- 0; ybar <- 1.22
phi_theta_0 <- phi_theta_mean(0, zbar, ybar, const)
phi_theta_1 <- phi_theta_mean(1, zbar, ybar, const)
plt_case_3 <- plot_elpd_phi_theta(phi_theta_0 - 0.03, phi_theta_1 + 0.03, zbar, ybar)

# Combine plots
plt_case_1 + plt_case_3 + plt_case_2 +
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

# Save plot
ggsave("figures/optim_elpd_phi_theta_cases.png", width = 16, height = 5, dpi = 300, units = "cm")
