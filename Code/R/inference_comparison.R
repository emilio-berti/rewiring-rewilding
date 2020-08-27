library(tidyverse)
library(quantreg)
library(cowplot)
library(vroom)

ranges <- function(pars){
  df <- interactions %>% 
    group_by(Predator) %>% 
    ungroup() %>% 
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

setwd("/home/GIT/Trophic_restoration")

phy <- vroom("Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv", delim = ",")

# all predators, using MammalDIET database
# want to keep this as constant for late filtering
Predators <- vroom("Data/MammalDIET.1.0.txt") %>%
  filter(Animal == 1, Plant == 0, MammalEater == 1) %>% 
  mutate(Binomial = paste(Genus, Species, sep = "_")) %>% 
  select(Binomial, Animal, Plant) %>% 
  pull(Binomial)

interactions <- read_csv("Results/inference_comparison.csv") %>% 
  mutate(`Pred Mass` = log10(`Pred Mass`),
         `Prey Mass` = log10(`Prey Mass`)) %>% 
  mutate(Dataset = "GLOBI")

#####################################
#   more interactions ###############
Serengeti_1 <- vroom("Data/Food_webs/Baskerville_cleaned.csv") %>% 
  transmute(Predator = predator, Prey = prey) %>% 
  mutate(Dataset = "Baskerville")

Serengeti_2 <- vroom("Data/Food_webs/deVisser_cleaned.csv") %>% 
  mutate(Dataset = "deVisser")

ECOWeB <- vroom("Data/Food_webs/ECOWEB_cleaned.csv") %>% 
  mutate(Dataset = "ECOWeB")

# merge all datasets and keep only predators of mammals
interactions <- interactions %>% 
  full_join(Serengeti_1) %>% 
  full_join(Serengeti_2) %>% 
  full_join(ECOWeB) %>% 
  filter(Predator %in% gsub("_", " ", Predators))

# get predators body masses
pred_mass <- interactions$Predator %>% 
  map(function(x){
    phy %>% 
      filter(Binomial.1.2 == gsub(" ", "_", x)) %>% 
      pull(Mass.g) %>% 
      log10()
  }) %>% 
  unlist()

# get prey body masses
prey_mass <- interactions$Prey %>% 
  map(function(x){
    phy %>% 
      filter(Binomial.1.2 == gsub(" ", "_", x)) %>% 
      pull(Mass.g) %>% 
      log10()
  }) %>% 
  unlist()

# add body masses to tibble
interactions <- interactions %>% 
  mutate(`Pred Mass` = pred_mass,
         `Prey Mass` = prey_mass)
#####################################
pars <- runif(2)
niche <- optim(par = pars, fn = ranges, method = "L-BFGS-B", lower = c(0, 0))

center <- lm(`Prey Mass` ~ `Pred Mass`, data = interactions)
upper <- rq(`Prey Mass` ~ `Pred Mass`, data = interactions, tau = 0.05)
lower <- rq(`Prey Mass` ~ `Pred Mass`, data = interactions, tau = 0.95)

interactions %>% 
  ggplot() +
  aes(`Pred Mass`, `Prey Mass`) +
  geom_point(aes(col = Dataset, size = Dataset)) +
  geom_abline(aes(intercept = center$coefficients[1], slope = center$coefficients[2]), linetype = 2) + 
  geom_abline(aes(intercept = upper$coefficients[1], slope = upper$coefficients[2]), linetype = 1) +
  geom_abline(aes(intercept = lower$coefficients[1], slope = lower$coefficients[2]), linetype = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line())  +
  scale_color_manual(values = wesanderson::wes_palette("Moonrise2")) +
  ggtitle("Quantile regression") -> p1

interactions %>% 
  ggplot() +
  aes(`Pred Mass`, `Prey Mass`) +
  geom_point(aes(col = Dataset, size = Dataset)) +
  geom_abline(aes(intercept = 0, slope = niche$par[1]), linetype = 2) +
  geom_abline(aes(intercept = 0, slope = niche$par[1] - niche$par[2]), linetype = 1) +
  geom_abline(aes(intercept = 0, slope = niche$par[1] + niche$par[2]), linetype = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line()) +
  scale_color_manual(values = wesanderson::wes_palette("Moonrise2")) +
  ggtitle("Optimization algorithm") -> p2

plot_grid(p1, p2)

interactions %>% 
  group_by(Dataset) %>% 
  tally() %>% 
  ggplot() +
  geom_bar(aes(x = "", y = n, fill = Dataset), width = 1, stat = "identity") +
  coord_polar("y") +
  xlab("") +
  ylab("") +
  theme(axis.line = element_blank())
