####################
# Checking the memory model against static binomial model



# Global params
trials <- 100                        # Number of runs in each simulation.
n_simulations <- 50                  # Number of runs per probability to average out noise
beta <- 0.7                          # Weight of new information in memory update
probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # Varying probabilities

# Empty dataframe to store results
results <- tibble()

# Loop over different probabilities and run multiple simulations
for (p in probs) {
  cumrate_matrix <- matrix(NA, nrow = n_simulations, ncol = trials)  # Store runs
  for (sim in 1:n_simulations) {
    list_cumrate <- c()
    cumulativerate <- 0.5
    
    for (i in 1:trials) {
      coin <- rbinom(1, 1, prob = p) 
      cumulativerate <- cumulativerate * beta + coin * (1 - beta)
      list_cumrate[i] <- cumulativerate
    }
    
    cumrate_matrix[sim, ] <- list_cumrate  # Store each run
  }
  
  # Compute average across simulations
  mean_cumrate <- colMeans(cumrate_matrix)
  
  # Store in results
  df <- tibble(
    trial = 1:trials,
    prob = p,
    avg_list_cumrate = mean_cumrate
  )
  
  results <- bind_rows(results, df)
}


# Plot average estimated probability vs actual probability
ggplot(results, aes(x = trial, y = avg_list_cumrate, color = factor(prob))) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = prob, color = factor(prob)), linetype = "dashed") +
  labs(title = "Average Estimated Probability vs Actual Probability - Mean of 50 simulations",
       x = "Trials", y = "Mean probability", color = "True Probability") +
  ylim(0, 1) +
  theme_minimal()






#######################
# Following the sin-wave - a non-static example


# Parameters
trials <- 200      # Number of trials
beta <- 0.9        # Memory decay factor
T <- 25           # Period of sine wave

n_simulations <- 50  # Number of simulations to average out noise

# Empty dataframe to store results
results <- tibble()

# Run multiple simulations
cumrate_matrix <- matrix(NA, nrow = n_simulations, ncol = trials)  # Store runs

for (sim in 1:n_simulations) {
  list_cumrate <- c()
  cumulativerate <- 0.5
  
  for (i in 1:trials) {
    prob_i <- 0.5 + 0.4 * sin(2 * pi * i / T)  # Dynamic probability
    coin <- rbinom(1, 1, prob = prob_i)
    
    # Update estimated probability
    cumulativerate <- cumulativerate * beta + coin * (1 - beta)
    list_cumrate[i] <- cumulativerate
  }
  
  cumrate_matrix[sim, ] <- list_cumrate  # Store each run
}

# Compute average across simulations
mean_cumrate <- colMeans(cumrate_matrix)

# Store in results
results <- tibble(
  trial = 1:trials,
  avg_list_cumrate = mean_cumrate,
  true_prob = 0.5 + 0.4 * sin(2 * pi * (1:trials) / T)  # True probability function
)

# Plot estimated probability vs. actual probability
ggplot(results, aes(x = trial)) +
  geom_line(aes(y = avg_list_cumrate, color = "Estimated Probability"), size = 1) +
  geom_line(aes(y = true_prob, color = "True Probability"), linetype = "dashed", size = 1) +
  labs(title = "Memory Model Approximation of a Sinus Probability - Beta = 0.9",
       x = "Trials", y = "Probability") +
  ylim(0, 1) +
  scale_color_manual(values = c("Estimated Probability" = "blue", "True Probability" = "red")) +
  theme_minimal()







