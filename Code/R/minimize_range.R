ranges <- function(pars, dataset){
  df <- dataset %>% 
    select(`Pred Mass`, `Prey Mass`) %>% 
    log10() %>% 
    transmute(x = `Pred Mass`,
              y = `Prey Mass`) %>% 
    unique() %>% 
    group_by(x) %>% 
    mutate(z = max(abs(x * pars[1] - y))) %>% 
    ungroup()
  center_minimize <- sum((df$x * pars[1] - df$y)^2)
  radius_minimize <- sum((abs(df$z) - df$x * pars[2])^2)
  sum(center_minimize + radius_minimize)
}