data{
  int<lower=1> N;
  int<lower=1> ncov;
  int y[N];
  matrix[N,ncov] x;
  real<lower = 0> sigma_indic;
  real mu_indic;
  real<lower = 0> tau;
}

parameters{
  vector[ncov] beta_raw;
  vector[ncov] indic_raw;
  real beta_0;
}

transformed parameters{
  vector[ncov] beta;
  vector<lower=0,upper=1>[ncov] indic;
  
  indic = inv_logit(mu_indic + sigma_indic*indic_raw);
  
  beta = tau * indic .* beta_raw;
  
}

model{
  beta_raw ~ normal(0,1);
  
  indic_raw ~ normal(0,1);
  
  y ~ bernoulli_logit(beta_0 + x*beta);
  
}




parameters {
  real logit_alpha;
  ...
transformed parameters {
  real alpha = inv_logit(logit_alpha);
  ...
model {
  logit_alpha ~ normal(mu, sigma);
  
  
  
  
data{
  // int<lower=1> n_levels; //number of trophic levels
  // int<lower=1> n_ecozones; //number of ecozones
  // int<lower=1> n_scenarios; //number of scenarios
  real<lower=0, upper=1> proportions[2]; //proportions of each trophic level
  real<lower=0> sigma[2]; //standard error of effect estimates
}

parameters {
  real mu; //population scenario effect
  real<lower=0> tau; //population deviation
  vector[5] eta; //unscaled deviation from mu by trophic level
  vector[6] theta; //unscaled deviation from mu by ecozone
}

transformed parameters {
  real<lower=0, upper=1> theta = 
 
}

model {
  mu ~ normal(0, 1); //prior for mu
  sigma ~ gamma(1, 1); //prior for sigma
  //likelihood for each trophic level
  for (i in 1:TL){
    a ~ normal(mu, sigma); //normal
    Proportion[i] ~ 1 / (1 + exp(-a)); //logit-normal
  }
}