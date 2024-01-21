library(ggplot2)

# Parameters
r <- 0.05    # Risk-free rate
T <- 1       # Time to maturity
S0 <- 100    # Initial asset price
sigma <- 0.2 # Volatility
mu <- r      # Drift rate
num_levels <- 5  # Number of MLMC levels

# Function to simulate a single GBM path
simGBM <- function(S0, mu, sigma, T, numSteps) {
  dt <- T / numSteps
  muT <- (mu - sigma^2 / 2) * dt
  sigmaT <- sqrt(dt) * sigma
  t <- seq(0, T, length.out = numSteps + 1)
  W <- c(0, cumsum(rnorm(numSteps) * sqrt(dt))) # Brownian motion
  S <- S0 * exp((muT - sigmaT^2 / 2) * t + sigmaT * W)
  return(data.frame(Time = t, AssetPrice = S))
}

# Plot data
plot_data <- data.frame()

# True GBM path with fine grid
true_path <- simGBM(S0, mu, sigma, T, 2^(num_levels + 2))  # Fine grid for true path
true_path$Level <- 'True Path'
plot_data <- rbind(plot_data, true_path)

# GBM paths for each MLMC level
for (level in 0:(num_levels-1)) {
  numSteps <- 2^level * 10  # Coarser grid for each MLMC level
  path <- simGBM(S0, mu, sigma, T, numSteps)
  path$Level <- paste('Level', level)
  plot_data <- rbind(plot_data, path)
}

# Plot the GBM paths
ggplot(plot_data, aes(x = Time, y = AssetPrice, color = Level, group = Level)) +
  geom_line() +
  labs(title = "GBM Paths at Different Levels",
       x = "Time",
       y = "Asset Price",
       color = "Legend") +
  theme_minimal()
