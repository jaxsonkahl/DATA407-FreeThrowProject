setwd("/Users/jaxsonkahl/Desktop/UBCO/WINTER 26/COSC 407 Final Project/")

par(
  col.axis = "#333333",
  col.lab = "#333333",
  col.main = "#333333",
  cex.main = 1.2,
  cex.lab = 1.1
)

nba <- read.csv("PlayerData.txt")

nba <- nba[, c("Player", "FT", "FTA", "FT.")]
colnames(nba)[4] <- "FT_percent"

# players with enough attempts
nba <- subset(nba, FTA >= 50)

# removing duplicate player
nba <- nba[!duplicated(nba$Player), ]

head(nba)

player_name <- "Stephen Curry"

player_data <- subset(nba, Player == player_name)

p <- player_data$FT_percent[1]
N <- player_data$FTA[1]

p
N

# Simulation Function

simulate_ft <- function(p, n, reps = 1000) {
  makes <- rbinom(reps, n, p)
  estimates <- makes / n
  return(estimates)
}

# Sampling Distribution

estimates_10 <- simulate_ft(p, n = 100, reps = 1000)

hist(estimates_10,
     col = "#4C72B0",
     border = "white",
     main = paste("Sampling Distribution of FT% Estimate\n", player_name, "(n = 10)"),
     xlab = "Estimated Free Throw Percentage",
     ylab = "Frequency")

abline(v = p, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = "True FT%",
       col = "red",
       lty = 2,
       lwd = 2)


# Variance vs Sample Size

sample_sizes <- c(10, 25, 50, 100, 200)
reps <- 1000

variances <- c()
mean_estimates <- c()
theoretical_var <- c()

for (n in sample_sizes) {
  
  estimates <- simulate_ft(p, n, reps)
  
  variances <- c(variances, var(estimates))
  mean_estimates <- c(mean_estimates, mean(estimates))
  theoretical_var <- c(theoretical_var, p * (1 - p) / n)
  
}

results <- data.frame(
  SampleSize = sample_sizes,
  MeanEstimate = mean_estimates,
  SimulatedVariance = variances,
  TheoreticalVariance = theoretical_var
)

print(results)


plot(sample_sizes, variances,
     type = "b",
     pch = 19,
     col = "#4C72B0",
     lwd = 2,
     xlab = "Sample Size",
     ylab = "Variance of Estimate",
     main = paste("Variance of FT% Estimate vs Sample Size\n", player_name))

lines(sample_sizes, theoretical_var,
      type = "b",
      pch = 17,
      lty = 2,
      col = "#DD8452",
      lwd = 2)

legend("topright",
       legend = c("Simulated Variance", "Theoretical Variance"),
       col = c("#4C72B0", "#DD8452"),
       pch = c(19,17),
       lty = c(1,2),
       lwd = 2)


# Confidence Interval Widths

ci_widths <- c()

for (n in sample_sizes) {
  
  estimates <- simulate_ft(p, n, reps)
  
  lower <- estimates - 1.96 * sqrt(estimates * (1 - estimates) / n)
  upper <- estimates + 1.96 * sqrt(estimates * (1 - estimates) / n)
  
  lower[lower < 0] <- 0
  upper[upper > 1] <- 1
  
  widths <- upper - lower
  
  ci_widths <- c(ci_widths, mean(widths, na.rm = TRUE))
  
}

ci_results <- data.frame(
  SampleSize = sample_sizes,
  Avg_CI_Width = ci_widths
)

print(ci_results)


plot(sample_sizes, ci_widths,
     type = "b",
     pch = 19,
     col = "#55A868",
     lwd = 2,
     xlab = "Sample Size",
     ylab = "Average 95% CI Width",
     main = paste("Confidence Interval Width vs Sample Size\n", player_name))



# CLT / Larger Sample Histogram 

estimates_100 <- simulate_ft(p, n = 100, reps = 1000)

hist(estimates_100,
     col = "#55A868",
     border = "white",
     main = paste("Sampling Distribution of FT% Estimate\n", player_name, "(n = 100)"),
     xlab = "Estimated Free Throw Percentage",
     ylab = "Frequency")

abline(v = p, col = "red", lwd = 2, lty = 2)


# Coverage of 95% CI

coverage_rates <- c()

for (n in sample_sizes) {
  
  estimates <- simulate_ft(p, n, reps)
  
  lower <- estimates - 1.96 * sqrt(estimates * (1 - estimates) / n)
  upper <- estimates + 1.96 * sqrt(estimates * (1 - estimates) / n)
  
  lower[lower < 0] <- 0
  upper[upper > 1] <- 1
  
  covered <- (p >= lower) & (p <= upper)
  
  coverage_rates <- c(coverage_rates, mean(covered, na.rm = TRUE))
  
}

coverage_results <- data.frame(
  SampleSize = sample_sizes,
  CoverageRate = coverage_rates
)

print(coverage_results)


plot(sample_sizes, coverage_rates,
     type = "b",
     pch = 19,
     col = "#C44E52",
     lwd = 2,
     ylim = c(0.85, 1.00),
     xlab = "Sample Size",
     ylab = "Coverage Rate",
     main = paste("Coverage of 95% Confidence Intervals\n", player_name))

abline(h = 0.95, lty = 2)


# Multi-player Comparison 
players <- c("Stephen Curry",
             "Damian Lillard",
             "Kevin Durant",
             "Giannis Antetokounmpo")

comparison <- data.frame()

for (pl in players) {
  
  temp <- subset(nba, Player == pl)
  
  p_pl <- temp$FT_percent[1]
  
  for (n in sample_sizes) {
    
    estimates <- simulate_ft(p_pl, n, reps)
    
    comparison <- rbind(comparison,
                        data.frame(
                          Player = pl,
                          SampleSize = n,
                          MeanEstimate = mean(estimates),
                          SimulatedVariance = var(estimates),
                          TheoreticalVariance = p_pl * (1 - p_pl) / n
                        ))
    
  }
  
}

print(comparison)


# Bootstrap 
ft_vector <- c(rep(1, round(player_data$FT[1])),
               rep(0, round(player_data$FTA[1] - player_data$FT[1])))

bootstrap_means <- replicate(
  1000,
  mean(sample(ft_vector, size = 50, replace = TRUE))
)

hist(bootstrap_means,
     col = "#8172B2",
     border = "white",
     main = paste("Bootstrap Distribution of FT% Estimate\n", player_name, "(n = 50)"),
     xlab = "Bootstrap FT%",
     ylab = "Frequency")

abline(v = mean(ft_vector), col = "red", lwd = 2, lty = 2)

sd(bootstrap_means)

