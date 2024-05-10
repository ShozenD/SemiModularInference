# Import libraries
library(yaml)
library(dplyr)
library(furrr)
library(mvtnorm)
library(devtools)
library(optparse)
load_all()

# Load experiment configurations
cmdln_parser <- OptionParser()
cmdln_parser <- add_option(cmdln_parser, c("-c", "--config"), type = "character", dest = "config_name", default = "config_original", help = "name of config file")
cmdln_args <- parse_args(cmdln_parser)
config <- read_yaml(paste0("config/", cmdln_args$config_name, ".yaml"))

# Constants
const <- list(
  n = config$experiment$n,
  m = config$experiment$n,
  s2z = config$experiment$s2z,
  s2y = config$experiment$s2y,
  s2t = config$experiment$s2t
)

# Generate grid
x1 <- seq(-1, 1, 0.002)
x2 <- seq(0.5, 1.5, 0.002)
grid <- as.matrix(expand.grid(x1, x2))

# Execute grid search
future::plan(multicore)
eta_optim <- future_map2_dbl(grid[,1], grid[,2], ~{
  optim(
    par = 0.01,
    fn = elpd_phi_theta_2,
    method = "L-BFGS-B",
    x1 = .x,
    x2 = .y,
    const = const,
    lower = 0.0005,
    upper = 1,
    control = list(fnscale = -1, ndeps = 1e-4)
  )$par
}, .progress = TRUE)

# Save results in a data.frame
df_eta_optim <- data.frame(x1 = grid[,1], x2 = grid[,2], eta = log10(eta_optim))

# Export results
fname <- paste0("optim_eta_2_", config$name, ".rds")
outpath <- file.path("results", fname)
saveRDS(df_eta_optim, outpath)
