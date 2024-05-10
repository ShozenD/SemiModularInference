# Load libraries
library(data.table)
library(ggplot2)
theme_set(theme_bw())

# Fixed parameters
n <- 25; m <- 50;
phi_true <- 0; theta_true <- 1;
s2z <- 4; s2y <- 1;

# Simulated data
set.seed(0)
Z <- rnorm(n, phi_true, sqrt(s2z))
Y <- rnorm(m, phi_true + theta_true, sqrt(s2y))

df_z <- data.table(x = Z, var = "Z")
df_y <- data.table(x = Y, var = "Y")

# Prior
s2t <- 0.5

x <- seq(-7, 7, length.out = 1e3)
z <- dnorm(x, phi_true, sqrt(s2z))
y <- dnorm(x, phi_true + theta_true, sqrt(s2y))
df_density <- data.table(x, z, y)

ggplot(df_density, aes(x)) +
  geom_line(aes(y = z, color = "Z")) +
  geom_line(aes(y = y, color = "Y")) +
  geom_point(data = df_z, aes(y = 0, color = var), alpha = 0.3) +
  geom_point(data = df_y, aes(y = 0, color = var), alpha = 0.3) +
  ggsci::scale_colour_nejm(labels = c(expression(italic(X)[1]), expression(italic(X)[2]))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-6, 6, 2)) +
  labs(y = "Density", color = "Variable") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.125, 0.8)
  )

ggsave("./figures/bias_normal_data.png", width = 8, height = 5, dpi = 300, units = "cm")
