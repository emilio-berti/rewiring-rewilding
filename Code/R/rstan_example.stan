data {
  real Y[10]; //heights for 10 people
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model{
  Y ~ normal(mu, sigma); //likelihood
  mu ~ normal(0, 1); //prior for mu
  sigma ~ gamma(1, 1); //prior for sigma
}