#' DONE
#' This function computes where the habitat suitability (HS) is higher than
#' random expectation, following the continuous Boyce index (CBI).
#' @param dataset is the data.frame with habitat suitability (HS) and
#'   predicted-to-expected ratio (PE).
#' @param res is the total resolution along which compute the threshold.
#' @return the value at which HS is higher than random expectation.
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