# Import libraries
library(dplyr)
library(ggplot2)
library(mvtnorm)
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

# Load data
df_eta_optim <- readRDS("results/optim_eta_2_extreme.rds")

# Slice along x1
x1_cut <- -0.25

df_x1 <- df_eta_optim %>%
  filter(between(x1, x1_cut, x1_cut + 0.001))
df_x1$elpd <- elpd_phi_2(10^df_x1$eta, df_x1$x1, df_x1$x2, const = const)

plt_x1_slice <- ggplot(df_x1, aes(x = x2, y = elpd)) +
  geom_line(aes(color = eta), linewidth = 0.7, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  viridis::scale_color_viridis(option = "D",
                               limits = c(min(df_eta_optim$eta),
                                          max(df_eta_optim$eta))) +
  labs(y = "ELPD",
       x = expression(bar(x)[2]),
       color = expression(log[10](eta)),
       subtitle = expression(bar(x)[1] == -0.25))

# Slice along x2
x2_cut <- 1.2

df_x2 <- df_eta_optim_phi %>%
  filter(between(x2, x2_cut, x2_cut + 0.001))

df_x2$elpd <- elpd_phi_2(10^df_x2$eta, df_x2$x1, df_x2$x2, const = const)

plt_x2_slice <- ggplot(df_x2, aes(x = x1, y = elpd)) +
  geom_line(aes(color = eta), linewidth = 0.7, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  viridis::scale_color_viridis(option = "D",
                               limits = c(min(df_eta_optim_phi$eta),
                                          max(df_eta_optim_phi$eta))) +
  labs(y = "ELPD",
       x = expression(bar(x)[1]),
       color = expression(log[10](eta)),
       subtitle = expression(bar(x)[2] == 1.12))

# ===== Heatmap =====
# Make contour plot
# Parameters for the bivariate normal distribution
mean <- c(0, 1)
sigma <- matrix(c(4/n, 0, 0, 1/m), ncol = 2)

# Generate grid
x <- seq(-1, 1, length.out = 100)
y <- seq(0.5, 1.5, length.out = 100)
grid <- expand.grid(x = x, y = y)

# Calculate density
grid$z <- dmvnorm(as.matrix(grid), mean = mean, sigma = sigma)

plt_heatmap <- ggplot(df_eta_optim, aes(x1, x2)) +
  geom_raster(aes(fill = eta), interpolate = TRUE) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_vline(xintercept = x1_cut, color = "white") +
  geom_hline(yintercept = x2_cut, color = "white") +
  geom_contour(data = grid, aes(x = x, y = y, z = z), color = "#de425b") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0.5, 1.5)) +
  viridis::scale_fill_viridis(option = "D") +
  labs(x = expression(bar(x)[1]),
       y = expression(bar(x)[2]),
       fill = expression(log[10](eta)),
       subtitle = bquote("Optimal" ~ eta ~ "on" ~ Y[1]))

plt_heatmap + plt_x1_slice + plt_x2_slice +
  plot_layout(guides = 'collect', axes = "collect") &
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

ggsave("figures/optim_eta_heatmap_phi_theta_extreme.png", width = 16, height = 6, dpi = 300, units = "cm")
