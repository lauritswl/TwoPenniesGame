####################
# Checking the Reinforcement_learning model against static binomial model
## Defining functions:
# Define Softmax function
softmax_function <- function(x, tau=1) {
  #' @param tau is a temperature. [0.00001, inf]. As tau approximates 0, the model will always pick the highest prob choice
  #' @param x is a vector input containing an abitrary value for each choice, defined by rl-algorithm.
  #' @returns a list of probabilities summing to 1.
  exp_x <- exp((x - max(x)) / tau)  # Subtract max(x) for numerical stability
  return(exp_x / sum(exp_x))
}

# Define Reinforcement Learning Agent
agent_rl <- function(RL_value, alpha, tau, gamma = 1) {
  #' @param value; 
  # Exploration - Turned off by default
  if(gamma < runif(1)){
    choice <- rbinom(1, 1, 0.5)
    return(choice)
  }
  # If sticking to rules
  probs <- softmax_function(RL_value, tau) # Using reinforcement values and temprature to get probabilities.
  choice <- rbinom(1, 1, probs)
  return(probs)
}

update_rl_values <- function(RL_value, alpha, choice, feedback) {
  #' @param RL_value is a vector of two RL_values. As they approach 1, they are updated slower.
  #' @param alpha is the learning rate. Defining how quickly we update RL_value
  #' @param choice is the chosen state of the RL_agent this turn. It is used as a mathemathical if-statement.
  #' @param feedback is the output of the game. Was the correct hand guessed this round?
  #' 
  v1 <- RL_value[1] + alpha * (choice) * (feedback - RL_value[1])
  v2 <- RL_value[2] + alpha * (1-choice) * (feedback - RL_value[2])
  RL_value <- c(v1, v2)
  return(RL_value)
}


# Global params
trials <- 100                        # Number of runs in each simulation.
n_simulations <- 50                  # Number of runs per probability to average out noise
alpha <- 0.7                         # Learning rate
tau <-  0.3                          # temperature - how deterministic is the model
gamma <- 0.95                        # exploration
probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # Varying probabilities

# Empty dataframe to store results
results <- tibble()

# Loop over different probabilities and run multiple simulations
for (p in probs) {
  prob_matrix <- matrix(NA, nrow = n_simulations, ncol = trials)  # Store runs
  for (sim in 1:n_simulations) {
    list_rl_prob <- c()
    RL_value <- c(0.5,0.5)
    
    for (i in 1:trials) {
      coin <- rbinom(1, 1, prob = p) 
      list_rl_prob[i] <- agent_rl(RL_value, alpha, tau, gamma)
      guess <- rbinom(1, 1, list_rl_prob[i])
      feedback <- ifelse(coin == guess, yes = 1, no = 0)
      RL_value <- update_rl_values(RL_value, alpha, choice = guess, feedback = feedback)
    }
    
    prob_matrix[sim, ] <- list_rl_prob  # Store each run
  }
  
  # Compute average across simulations
  mean_prob <- colMeans(prob_matrix)
  
  # Store in results
  df <- tibble(
    trial = 1:trials,
    prob = p,
    avg_list_prob = mean_prob
  )
  
  results <- bind_rows(results, df)
}


# Plot average estimated probability vs actual probability
ggplot(results, aes(x = trial, y = avg_list_prob, color = factor(prob))) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = prob, color = factor(prob)), linetype = "dashed") +
  labs(title = "Average Estimated Probability vs Actual Probability - Mean of 50 simulations",
       x = "Trials", y = "Mean probability", color = "True Probability") +
  ylim(0, 1) +
  theme_minimal()






#######################
# Following the sin-wave - a non-static example
# Global Parameters
trials <- 200       # Number of trials
T_period <- 25      # Period of the sine wave
n_simulations <- 50 # Number of simulations to average out noise
alpha <- 0.7        # Learning rate
tau <-  0.3         # Temperature (determinism)
gamma <- 0.95       # Exploration probability

# Empty dataframe to store results
results <- tibble()

# Run multiple simulations
prob_matrix <- matrix(NA, nrow = n_simulations, ncol = trials)  # Store runs

for (sim in 1:n_simulations) {
  list_rl_prob <- c()
  RL_value <- c(0.5, 0.5)  # Initial RL values
  
  for (i in 1:trials) {
    prob_i <- 0.5 + 0.4 * sin(2 * pi * i / T_period)  # Dynamic probability
    coin <- rbinom(1, 1, prob = prob_i)  # True outcome
    
    # RL agent makes a decision
    list_rl_prob[i] <- agent_rl(RL_value, alpha, tau, gamma)
    guess <- rbinom(1, 1, list_rl_prob[i])
    
    # Compute feedback & update RL values
    feedback <- ifelse(coin == guess, yes = 1, no = 0)
    RL_value <- update_rl_values(RL_value, alpha, choice = guess, feedback = feedback)
  }
  
  prob_matrix[sim, ] <- list_rl_prob  # Store each run
}

# Compute average across simulations
mean_prob <- colMeans(prob_matrix)

# Store in results
results <- tibble(
  trial = 1:trials,
  avg_list_prob = mean_prob,
  true_prob = 0.5 + 0.4 * sin(2 * pi * (1:trials) / T_period)  # True probability function
)

# Plot RL agent’s estimated probability vs. true probability
ggplot(results, aes(x = trial)) +
  geom_line(aes(y = avg_list_prob, color = "RL Estimated Probability"), size = 1) +
  geom_line(aes(y = true_prob, color = "True Probability"), linetype = "dashed", size = 1) +
  labs(title = "RL Model Approximation of a Sin-wave Probability",
       x = "Trials", y = "Probability") +
  ylim(0, 1) +
  scale_color_manual(values = c("RL Estimated Probability" = "blue", "True Probability" = "red")) +
  theme_minimal()





