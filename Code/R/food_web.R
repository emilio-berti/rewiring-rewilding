# This script generates the complete food web for all mammals using the
# phylogeny (Family) and traits (body mass). This is done once and then
# subsampled for each protected area removing all species that do not occurr
# within the boundaries.

library(tidyverse)
library(rglobi)
library(quantreg) #for Gravel method
library(rgbif)

setwd("/home/GIT/Trophic_restoration")

GLOBI <- list.files("Data/interactions")

phy <- read_csv("Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv", col_types = cols())

# get all mammals under study
marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")
Mammals <- phy %>% 
  filter(
    !IUCN.Status.1.2 %in% c("EX", "EW", "EP"),
    !Family.1.2 %in% marine_families,
    Binomial.1.2 != "Ursus_maritimus",
    Genus.1.2 != "Homo",
    Order.1.2 != "Chiroptera"
  ) %>% 
  pull(Binomial.1.2) %>% 
  gsub("_", " ", .)

# import GLOBI interactions file and clean it with rgbif
Globi <- read_csv("Data/interactions/all_mammals.csv", col_types = cols(), col_names = c("Pred", "Prey")) %>% 
  mutate(Pred = modify(Pred, function(x) name_backbone(x)$species),
         Prey = modify(Prey, function(x){if(!is.null(y <- name_backbone(x)$species)){y} else{NA}}), #if no species takes NA
         Class = modify(Prey, function(x){if(!is.null(y <- name_backbone(x)$class)){y} else{NA}})) %>% #I already know that all predator are mammals
  filter(Class == "Mammalia",
         #!is.na(Class),
         !is.na(Prey)) #remove taxon without species information

# remove marine families
marine_species <- phy %>% 
  filter(Family.1.2 %in% marine_families) %>% 
  pull(Binomial.1.2) %>% 
  gsub("_", " ", .)

Globi <- Globi %>% 
  filter(!Pred %in% marine_species,
         !Prey %in% marine_species,
         Pred != "Homo sapiens",
         Pred != "Ursus maritimus")

# clean taxonomy
source("Code/R/clean_taxonomy.R")
if(!all(Globi$Pred %in% Mammals)){
  Globi$Pred <- gsub("_", " ", clean_taxonomy(Globi$Pred)[[1]])
}
# check all species with correct taxonomy
is_empty(Globi$Pred[which(!Globi$Pred %in% Mammals)])

if(!all(Globi$Prey %in% Mammals)){
  Globi$Prey <- gsub("_", " ", clean_taxonomy(Globi$Prey)[[1]])
  Globi <- Globi %>% filter(!is.na(Prey))
}
# check all species with correct taxonomy
is_empty(Globi$Prey[which(!Globi$Prey %in% Mammals)])

# create tibble for analyses
interactions <- tibble(Predator = Globi$Pred, Prey = Globi$Prey) %>% 
  full_join(read_csv("Data/Food_webs/Baskerville_cleaned.csv", col_types = cols()) %>% #Serengeti food web 1
              transmute(Predator = predator, Prey = prey)) %>% 
  full_join(read_csv("Data/Food_webs/deVisser_cleaned.csv", col_types = cols())) %>% #Serengeti food web 2
  full_join(read_csv("Data/Food_webs/ECOWEB_cleaned.csv", col_types = cols())) #food webs from EcoWeb database

# get predators body masses
pred_mass <- interactions$Predator %>% 
  map(function(x){
    phy %>% 
      filter(Binomial.1.2 == gsub(" ", "_", x)) %>% 
      pull(Mass.g)
  }) %>% 
  unlist()

# get prey body masses
prey_mass <- interactions$Prey %>% 
  map(function(x){
    phy %>% 
      filter(Binomial.1.2 == gsub(" ", "_", x)) %>% 
      pull(Mass.g)
  }) %>% 
  unlist()

# add body masses to tibble
interactions <- interactions %>% 
  mutate(`Pred Mass` = pred_mass,
         `Prey Mass` = prey_mass)

# Niche model -------------------------------------------------------------
# function to be minimized
source("Code/R/minimize_range.R")

# Phylo -------------------------------------------------------------------
# Check if optimization algorithm always converge to the same values.
lapply(1:100, function(i){
  pars <- runif(2)*i
  niche <- optim(par = pars, fn = ranges, dataset = interactions, method = "L-BFGS-B", lower = c(0, 0))
  cbind(Init = pars, Final = niche$par)
}) -> global_maximum

global_maximum <- as_tibble(do.call(rbind, global_maximum)) %>% 
  mutate(Par = factor(rep(c("center", "radius"), 100)))

global_maximum %>% 
  group_by(Par) %>% 
  summarize(
    mean(Init),
    mean(Final),
    sd(Init),
    sd(Final)
  ) %>% 
  write_csv("Results/convergence_optimization.csv", col_names = T)

niche <- optim(par = c(0, 0), fn = ranges, dataset = interactions, method = "L-BFGS-B", lower = c(0, 0))

filtered <- interactions %>% 
  select(`Pred Mass`, `Prey Mass`) %>% 
  expand(`Pred Mass`, `Prey Mass`) %>% 
  filter(log10(`Prey Mass`) > niche$par[1] * log10(`Pred Mass`) - niche$par[2] * log10(`Pred Mass`),
         log10(`Prey Mass`) < niche$par[1] * log10(`Pred Mass`) + niche$par[2] * log10(`Pred Mass`))

all <- interactions %>% 
  filter(Predator != Prey) %>% 
  mutate(Pred_family = modify(Predator, function(x){phy %>% filter(Binomial.1.2 == gsub(" ", "_", x)) %>% pull(Family.1.2)}),
         Prey_family = modify(Prey, function(x){phy %>% filter(Binomial.1.2 == gsub(" ", "_", x)) %>% pull(Family.1.2)}))

family_food_web <- all %>% 
  dplyr::select(Pred_family, Prey_family) %>% 
  unique() 

# add Globi family levels
globi_families <- sapply(unique(subset(phy, Binomial.1.2 %in% gsub(" ", "_", Mammals))$Family.1.2), function(x){
  y <- get_interactions(x, interaction.type = "preyedUponBy")$target_taxon_name
  if(is_empty(y)){
    return(NULL)
  } else{
    y %>% 
      unique() %>% 
      strsplit(" ") %>% 
      modify(function(z) z[1]) %>% 
      unlist() -> predators
    phy %>% 
      filter(Genus.1.2 %in% predators) %>% 
      pull(Family.1.2) %>% 
      unique() -> families
    return(families)
  }
})

for(i in 1:length(globi_families)){
  if(!is_empty(globi_families[[i]])){
    j <- which(family_food_web$Prey_family == names(globi_families)[i])
    add <- setdiff(globi_families[[i]], family_food_web$Pred_family[j])
    add <- setdiff(add, "Hominidae")
    add <- setdiff(add, names(globi_families)[i])
    if(!is_empty(add)){
      family_food_web %>% 
        rbind(tibble(Pred_family = add, Prey_family = names(globi_families)[i])) -> family_food_web
    }
  }
}

write_csv(family_food_web, "Results/phylogenetic_food_web.csv", col_names = T)

lower_r <- interactions %>% 
  dplyr::select(`Pred Mass`, `Prey Mass`) %>% 
  mutate(`Pred Mass` = log10(`Pred Mass`),
         `Prey Mass` = log10(`Prey Mass`)) %>% 
  unique() %>% 
  rq(`Prey Mass` ~ `Pred Mass`, data = ., tau = 0.10) %>% 
  broom::tidy()

upper_r <- interactions %>% 
  dplyr::select(`Pred Mass`, `Prey Mass`) %>% 
  mutate(`Pred Mass` = log10(`Pred Mass`),
         `Prey Mass` = log10(`Prey Mass`)) %>% 
  unique() %>% 
  rq(`Prey Mass` ~ `Pred Mass`, data = ., tau = 0.90) %>% 
  broom::tidy()

center_r <- interactions %>% 
  dplyr::select(`Pred Mass`, `Prey Mass`) %>% 
  mutate(`Pred Mass` = log10(`Pred Mass`),
         `Prey Mass` = log10(`Prey Mass`)) %>% 
  unique() %>% 
  lm(`Prey Mass` ~ `Pred Mass`, data = .) %>% 
  broom::tidy()

rbind(lower_r, upper_r) %>% 
  write_csv("Results/pars_gravel.csv", col_names = T)


# Method figure -----------------------------------------------------------------
library(cowplot)

p1 <- ggplot() +
  geom_point(data = interactions, aes(`Pred Mass`, `Prey Mass`), shape = 20, size = 2, alpha = 1) +
  scale_x_log10(breaks = 10^seq(1, 6), labels = function(x) format(x, scientific = F)) +
  scale_y_log10(breaks = 10^seq(1, 6), labels = function(x) format(x, scientific = F)) +
  xlab("Predator body mass (g)") +
  ylab("Prey body mass (g)") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 30, vjust = 0.8),
    axis.text.y = element_text(angle = 30, hjust = 0.8)
  ) +
  geom_point(data = interactions %>% expand(`Pred Mass`, `Prey Mass`), aes(`Pred Mass`, `Prey Mass`), shape = 1, size = 2, alpha = 0.05) +
  geom_abline(aes(intercept = 0, slope = niche$par[1]), linetype = 2, col = "tomato", size = 1) +
  geom_abline(aes(intercept = 0, slope = niche$par[1] - niche$par[2]), linetype = 1, col = "tomato", size = 1) +
  geom_abline(aes(intercept = 0, slope = niche$par[1] + niche$par[2]), linetype = 1, col = "tomato", size = 1) +
  ggtitle("Optimization algorithm")

p2 <- ggplot() +
  geom_point(data = interactions, aes(`Pred Mass`, `Prey Mass`), shape = 20, size = 2, alpha = 1) +
  scale_x_log10(breaks = 10^seq(1, 6), labels = function(x) format(x, scientific = F)) +
  scale_y_log10(breaks = 10^seq(1, 6), labels = function(x) format(x, scientific = F)) +
  xlab("Predator body mass (g)") +
  ylab("Prey body mass (g)") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 30, vjust = 0.8),
    axis.text.y = element_text(angle = 30, hjust = 0.8)
  ) +
  geom_point(data = interactions %>% expand(`Pred Mass`, `Prey Mass`), aes(`Pred Mass`, `Prey Mass`), shape = 1, size = 2, alpha = 0.05) +
  geom_abline(aes(intercept = lower_r[1, ]$estimate, slope = lower_r[2, ]$estimate), linetype = 1, col = "steelblue", size = 1) +
  geom_abline(aes(intercept = upper_r[1, ]$estimate, slope = upper_r[2, ]$estimate), linetype = 1, col = "steelblue", size = 1) +
  geom_abline(aes(intercept = center_r[1, ]$estimate, slope = center_r[2, ]$estimate), linetype = 2, col = "steelblue", size = 1) +
  ggtitle("Quantile regression")

plot_grid(p1, p2)

ggsave("Manuscript/Figures/niche_inference.svg", width = 7.44)
ggsave("Manuscript/Figures/niche_inference.pdf", width = 7.44)
ggsave("Manuscript/Figures/niche_inference.png", width = 7.44)