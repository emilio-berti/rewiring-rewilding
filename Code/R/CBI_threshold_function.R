CBI_Th <- function(dataset, res = 100){
  HS <- seq(min(dataset$HS), max(dataset$HS), length = 100)
  y <- rep(NA, 100)
  z <- rep(NA, 100)
  for(i in 1:100){
    x <- dataset$PE[dataset$HS > HS[i] & dataset$HS < HS[i + 1]]
    y[i] <- mean(x)
    z[i] <- sd(x)
  }
  CI <- y - 2 * z
  return(HS[which(CI > 1)[1]])
}