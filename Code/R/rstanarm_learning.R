library(rstan)
library(shinystan)
library(rstanarm)

x <- 1:100
g <- rep(1:2, each = 50)
y <- rnorm(100, g, 1)
dat <- data.frame(y = y, g = factor(g), r = 1:100)

model <- lm(y ~ g + r, data = dat)

fit <- stan_lm(
  y ~ g + r,
  data = dat,
  prior = R2(0.5)
)
