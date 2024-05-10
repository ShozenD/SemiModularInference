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

# Edge case 1
zbar <- -0.5; ybar <- -0.498;
phi_0 <- phi_mean(0, zbar, ybar, const)
phi_1 <- phi_mean(1, zbar, ybar, const)
plt_1 <- plot_elpd_phi(phi_0 - 0.0001, phi_1 + 0.0001, zbar, ybar)

# Edge case 2
zbar <- -0.5; ybar <- -0.4957;
phi_0 <- phi_mean(0, zbar, ybar, const)
phi_1 <- phi_mean(1, zbar, ybar, const)
plt_2 <- plot_elpd_phi(phi_0 - 0.0001, phi_1 + 0.0001, zbar, ybar)

# Case 1.1: When the ELPD is crossing over and when x1 and x2 is sufficiently close
zbar <- -0.3810; ybar <- -0.3805;
phi_0 <- phi_mean(0, zbar, ybar, const)
phi_1 <- phi_mean(1, zbar, ybar, const)
plt_3 <- plot_elpd_phi(phi_0 - 0.0001, phi_1 + 0.0001, zbar, ybar)

plt_1 + plt_2 + plt_3 +
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

ggsave("figures/optim_elpd_phi_edge_cases.png", width = 16, height = 5, dpi = 300, units = "cm")
