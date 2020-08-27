###
# The use of the log-ratio model with optimization algorithm is worthless is
# data is not good enough, because this bayesian approach train the parameter
# space on a network
### 

log_ratio <- function(x, y){
  alpha = 1
  beta = -1
  gamma = -1
  mu <- - beta / (2 * gamma)
  sigma <- sqrt(- 1 / (2 * gamma))
  logit <- alpha + beta * log10(y / x) + gamma * log10(y / x)^2
  return(logit)
}

x <- 1
y <- seq(10^-10, 10^10, length = 1000)

z <- y %>% 
  map(function(i) log_ratio(x, i)) %>% 
  unlist()

plot(log10(y / x), z)
