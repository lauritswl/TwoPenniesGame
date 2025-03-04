data{
  int<lower=1> n;
  array[n] int mm_choice; // Did the memory model choose left or right
  array[n] int other; // Did the opponent choose left or right
  array[n] int feedback; // Binary Feedback - is mm_choice == other
}

parameters{
  real<lower=0, upper=1> beta;   // Memory updating parameter - likelihood to overfit/underfit
  // real<lower=0, upper=1> gamma;     // Exploration parameter
  real<lower=0, upper=1> init_rate; // Initial cumulative rate
  
}

transformed parameters{
  vector[n] cumrate; // 
  cumrate[1] = init_rate; // set cumulativerate to 0.5
  for (trial in 2:n){ // Update cumrate
    cumrate[trial] = beta * cumrate[trial-1] + (1 - beta) * other[trial-1]; // Update cumulativerate with information form last trial
  }
}

// Defining the model:
model{
 // Priors
 target += beta_lpdf(beta | 2, 2);  // Equivalent to beta ~ beta(2,2), a non-informed prior
 target += beta_lpdf(init_rate | 2, 2); 
 
 // target += beta_lpdf(gamma | 2, 2)
 // Likelihood
 target += bernoulli_lpmf(mm_choice | cumrate); 
 
}

