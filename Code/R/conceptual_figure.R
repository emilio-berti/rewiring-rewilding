library(tidyverse)

source('my_theme.R')

normal <- function(x, mu, sigma) {
  y <- exp(- (x - mu)^2 / (2 * sigma^2))
  return(y)
}

diet <- tibble(
  x = seq(0.5, 9.5, length.out = 1000),
  X = normal(x, 2, 0.25),
  Y = normal(x, 4, 0.5),
  Z = normal(x, 6, 1),
  B = 0
) %>% 
  gather(key = 'species', value = 'n', -x, -B)

diet %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = B, ymax = n, fill = species), col ='black', alpha = 0.35) +
  monkey_bare +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_fill_manual(values = c('steelblue', 'steelblue', 'tomato')) +
  xlab('') +
  ylab('') +
  theme(
    axis.line.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank()
  )
  
