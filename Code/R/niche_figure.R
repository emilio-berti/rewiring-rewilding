quantile_model <- read_csv("../../Results/pars_gravel.csv", col_types = cols()) %>% 
  transmute(Parameter = term, Value = estimate, Quantile = tau)

predators <- tibble(Species = c('Acinonyx_jubatus', 'Canis_lupus', 'Panthera_leo', 'Smilodon_populator')) %>% 
  mutate(Mass = as.numeric(modify(Species, function(x) get_mass(x)$Mass))) %>% 
  arrange(Mass)

prey <- tibble(Species = c('Capreolus_capreolus', 'Cervus_elaphus', 'Alces_alces', 'Connochaetes_taurinus', 'Ceratotherium_simum', 'Loxodonta_africana')) %>% 
  mutate(Mass = as.numeric(modify(Species, function(x) get_mass(x)$Mass))) %>% 
  arrange(Mass)

X <- phy %>% 
  filter(Diet.Vertebrate > 0) %>% 
  pull(Mass.g) %>% 
  log10()

Y <- phy %>% 
  filter(Diet.Plant == 100) %>% 
  pull(Mass.g) %>% 
  log10()

samp <- tibble(
  X = X[sample(1000, 1000)], 
  Y = Y[sample(1000, 1000)], 
  Phyl = sample(c(TRUE, FALSE), 1000, replace = TRUE)
)

p0 <- tibble(
  x = seq(0.5, 6, length.out = 5),
  ymin = quantile_model$Value[1] + quantile_model$Value[2] * x,
  ymax = quantile_model$Value[3] + quantile_model$Value[4] * x
) %>% 
  ggplot() +
  geom_point(data = samp[1:50, ], aes(X, Y), alpha = 0.5) +
  scale_x_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  scale_y_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  xlab('Predator body mass (kg)') +
  ylab('Prey body mass (kg)') +
  monkey_theme 

p0

p1 <- tibble(
  x = seq(0.5, 6, length.out = 5),
  ymin = quantile_model$Value[1] + quantile_model$Value[2] * x,
  ymax = quantile_model$Value[3] + quantile_model$Value[4] * x
) %>% 
  ggplot() +
  geom_point(data = samp %>% filter(Phyl == TRUE), aes(X, Y), alpha = 0.5) +
  scale_x_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  scale_y_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  xlab('Predator body mass (kg)') +
  ylab('Prey body mass (kg)') +
  monkey_theme 

p1

p2 <- tibble(
  x = seq(0.5, 8, length.out = 5),
  ymin = quantile_model$Value[1] + quantile_model$Value[2] * x,
  ymax = quantile_model$Value[3] + quantile_model$Value[4] * x
) %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.25) +
  geom_point(data = samp %>% filter(Phyl == TRUE), aes(X, Y), alpha = 0.5) +
  scale_x_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  scale_y_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  xlab('Predator body mass (kg)') +
  ylab('Prey body mass (kg)') +
  monkey_theme 

p2

p3 <- tibble(
  x = seq(0.5, 8, length.out = 5),
  ymin = quantile_model$Value[1] + quantile_model$Value[2] * x,
  ymax = quantile_model$Value[3] + quantile_model$Value[4] * x
) %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.25) +
  geom_point(data = samp %>% filter(Y < X * 0.906 + 0.513, Y > 0.09 * X + 0.896), 
             aes(X, Y), alpha = 0.5) +
  scale_x_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  scale_y_continuous(breaks = 1:6, labels = 10^(1:6) / 100, expand = c(0, 0.05)) +
  xlab('Predator body mass (kg)') +
  ylab('Prey body mass (kg)') +
  monkey_theme 

p3

plot_grid(p0, p1, p2, p3, nrow = 2)

geom_vline(data = predators, aes(xintercept = log10(Mass))) +
  geom_hline(data = prey, aes(yintercept = log10(Mass))) +
  geom_text(data = predators, aes(x = log10(Mass), y = 0, label = Species), angle = 90, vjust = 0) +
  geom_text(data = prey, aes(x = 1, y = log10(Mass), label = Species), vjust = 0) +


