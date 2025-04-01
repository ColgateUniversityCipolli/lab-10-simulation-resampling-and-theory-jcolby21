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
