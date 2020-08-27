library(tidyverse)
source("get_mass.R")

phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv")
niche <- read_csv("../../Results/pars_gravel.csv")
phy_fw <- read_csv("../../Results/phylogenetic_food_web.csv")

predators <- phy %>% 
  filter(Family.1.2 == "Felidae") %>% 
  pull(Binomial.1.2) %>% 
  sample(40)

prey <- phy %>% 
  filter(Family.1.2 %in% c("Cervidae", "Bovidae", "Muridae")) %>% 
  pull(Binomial.1.2) %>% 
  sample(40)

phy_fw <- tibble(
  Predator = predators,
  Prey = prey
) %>% 
  expand(Predator, Prey) %>% 
  mutate(
    PredMass = sapply(Predator, function(x) get_mass(x)$Mass),
    PreyMass = sapply(Prey, function(x) get_mass(x)$Mass)
  ) %>% 
  dplyr::select(PredMass, PreyMass)

fw <- phy_fw %>% 
  filter(
    log10(PreyMass) > (0.896 + 0.0924 * log10(PredMass)),
    log10(PreyMass) < (0.513 + 0.906 * log10(PredMass))
  )

ggplot() +
  geom_point(data = phy_fw, aes(PredMass, PreyMass), alpha = 0.5, pch = 21, size = 1) +
  geom_point(data = fw, aes(PredMass, PreyMass), pch = 20, size = 1) +
  geom_abline(aes(intercept = 0.896, slope = 0.0924), linetype = "dashed") +
  geom_abline(aes(intercept = 0.513, slope = 0.906), linetype = "dashed") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gainsboro")
  ) +
  scale_x_log10(name = "Predator's mass (g)", labels = scales::comma) +
  scale_y_log10(name = "Prey's mass (g)", labels = scales::comma)

ggsave("../../Manuscript/Figures/niche_inference.png", width = 5, height = 4)
