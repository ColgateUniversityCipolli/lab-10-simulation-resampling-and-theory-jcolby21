#Basic Simulation
library(tidyverse)
library(ggplot2)
p_true <- 0.39  # True probability
n1 <- 1000  # Sample size for first simulation
n2 <- 2000  # Sample size for second simulation
num_polls <- 10000  # Number of simulated polls

# Sample size of 1000
polls1 <- rbinom(num_polls, n1, p_true) / n1  
df1 <- data.frame(proportion = polls1)  


ggplot(df1, aes(x = proportion)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  labs(title = "Sampling Distribution of Proportion (n = 1000)",
       x = "Sample Proportion",
       y = "Density") +
  theme_minimal()

# Middle 95% range for sample size = 1000
range_95_1 <- quantile(polls1, c(0.025, 0.975))
margin_of_error_1 <- diff(range_95_1) / 2
#Middle 95% of data is between 0.36 and 0.42
#Margin of error is approximately 0.03, Gallup reported 4% (0.04)

# Sample size of 2000
polls2 <- rbinom(num_polls, n2, p_true) / n2  
df2 <- data.frame(proportion = polls2)  

ggplot(df2, aes(x = proportion)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgreen", color = "white", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1.2) +
  labs(title = "Sampling Distribution of Proportion (n = 2000)",
       x = "Sample Proportion",
       y = "Density") +
  theme_minimal()

# Middle 95% range for sample size = 2000
range_95_2 <- quantile(polls2, c(0.025, 0.975))
margin_of_error_2 <- diff(range_95_2) / 2
# Middle 95% is between 0.369 and 0.411
# Margin of error is approximately 0.021, Gallup reported 2% (0.02)


#Resampling

n_survey <- 1000  # Sample size for Gallup survey 
#create survey data where 39% satisfied (1), rest are not (0)
survey_data <- c(rep(1, 0.39 * n_survey), rep(0, n_survey - 0.39 * n_survey))

num_resamples <- 10000  # Number of resamples
resample_proportions <- numeric(num_resamples)  # Initialize a vector to store proportions

for (i in 1:num_resamples) {
  resample <- sample(survey_data, n_survey, replace = TRUE)  # Resample with replacement
  resample_proportions[i] <- mean(resample)
}

df_resamples <- tibble(proportion = resample_proportions)

ggplot(df_resamples, aes(x = proportion)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1.2) +
  labs(title = "Sampling Distribution of Proportion (Resampling)",
       x = "Sample Proportion",
       y = "Density") +
  theme_minimal()

# Middle 95% range from resampling
range_95_resample <- quantile(df_resamples$proportion, c(0.025, 0.975))
margin_of_error_resample <- diff(range_95_resample) / 2
#Middle 95% was from 0.36 to 0.42
#Margin of error is approximately 0.03, Gallup reported 4% (0.04)


#Simulation over n and p

n_values <- seq(100, 3000, by = 10)  # Sample sizes from 100 to 3000
p_values <- seq(0.01, 0.99, by = 0.01)  # Probabilities from 0.01 to 0.99
num_simulations <- 10000  # Number of simulations


simulate_margin_of_error <- function(n, p, num_simulations) {
  
  sample_data <- rbinom(num_simulations, size = n, prob = p) / n
  
  # Calculate the 2.5th and 97.5th percentiles
  lower <- quantile(sample_data, 0.025)
  upper <- quantile(sample_data, 0.975)
  
  # Half the range between the 2.5th and 97.5th percentiles
  margin_of_error <- (upper - lower) / 2
  return(margin_of_error)
}

# Empty data frame
results <- expand.grid(n = n_values, p = p_values)

# Apply the simulation function to each combination of n and p
results$margin_of_error <- mapply(simulate_margin_of_error, results$n, results$p, MoreArgs = list(num_simulations = num_simulations))

ggplot(results, aes(x = n, y = p, fill = margin_of_error)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C", name = "Margin of Error") +
  labs(
    title = "Margin of Error vs. Sample Size and Probability",
    x = "Sample Size (n)",
    y = "Probability (p)"
  ) +
  theme_minimal()

#Actual Margin of Error Calculation

z <- 1.96  # 95% confidence level
n_values <- seq(100, 2000, by=10)  # Sample sizes
p_values <- seq(0.01, 0.99, by=0.01)  # Proportions

#data for each n and p
moe_data <- expand.grid(n = n_values, p = p_values) %>%
  mutate(
    moe = z * sqrt((n * p * (1 - p) + (z^2) / 4)) / (n + z^2)
  )

ggplot(moe_data, aes(x = n, y = p, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(name="Margin of Error") +
  labs(title="Wilson Margin of Error",
       x="Sample Size (n)",
       y="Proportion (p)") +
  theme_minimal()
