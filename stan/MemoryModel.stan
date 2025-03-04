data{
  int<lower=1> n;
  array[n] int mm_choice; // Did the memory model choose left or right
  array[n] int other; // Did the opponent choose left or right
}

parameters{
  real<lower=0, upper=1> beta;   // Memory updating parameter - likelihood to overfit/underfit
}

transformed parameters{
  vector[n] cumrate; // 
  cumrate[1] = 0.5; // set cumulativerate to 0.5
  for (trial in 2:n){ // Update cumrate
    cumrate[trial] = beta * cumrate[trial-1] + (1 - beta) * other[trial-1]; // Update cumulativerate with information form last trial
  }
}

// Defining the model:
model{
 // Priors
 target += beta_lpdf(beta | 2, 2);  // Equivalent to beta ~ beta(2,2), a non-informed prior
 // Likelihood
 target += bernoulli_lpmf(mm_choice | cumrate); 
}

