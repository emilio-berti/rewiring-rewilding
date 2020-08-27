library(rstan)
library(shinystan)
library(loo)

options(mc.cores = parallel::detectCores() - 1)

rstan_options(auto_write = TRUE)

Y <- rnorm(10, 0, 1)

fit <- stan(
  "rstan_example.stan", 
  data = list(Y = Y),
  iter = 1000,
  chains = 4
)

print(fit, probs = c(0.25, 0.5, 0.75))

mu = extract(fit,'mu')[[1]]
qplot(mu, fill = I("blue"))

afit <- as.shinystan(fit)
launch_shinystan(afit)
