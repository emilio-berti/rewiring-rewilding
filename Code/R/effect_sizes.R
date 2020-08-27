pooled_var <- function(x, y){
  (x + y) / 2 #in this case I know x and y have the same number of observations
}

CohenD <- function(x, y, sp){
  d <- (x - y) / sp
  return(d)
}
