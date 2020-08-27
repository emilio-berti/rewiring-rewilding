library(tidyverse)

source("select_species.R")

repl <- read_csv("../../Results/Replacements.csv")

repl %>% filter(Replacement == "No Replacement")

lost <- append(
  setdiff(extinct, repl$Extinct),
  repl$Extinct[repl$Replacement == "No Replacement"]
) %>% 
  unique()

replaced <- repl %>% 
  filter(Replacement != "No Replacement") %>% 
  filter(Extinct != "Extinct") %>% 
  pull(Extinct) %>% 
  unique()

any(replaced %in% lost) #check for consistency; this should be: FALSE
length(replaced) #number of species replaced
length(replaced) + length(lost) #number of species extinct

# families -----
marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")

phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv") %>% 
  filter(
    !Family.1.2 %in% marine_families, #excluding cetaceans and pinnipeds families
    Order.1.2 != "Chiroptera", #excluding bats
    IUCN.Status.1.2 %in% c("EW", "EX", "EP"), #excluding extinct species
    Binomial.1.2 != "Ursus_maritimus" #excluding polar bear
  )

phy %>% 
  mutate(Replaced = map(Binomial.1.2, function(x) {
    if (x %in% lost) {
      "NO"
    } else if (x %in% replaced) {
      "YES"
    } else {
      NA
    }
  }) %>% unlist()) %>% 
  group_by(Family.1.2, Replaced) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(key = "Replaced", value = "n", fill = 0) %>%
  # pull(Family.1.2) %>% 
  # unique() %>% 
  filter(YES > 0)
  knitr::kable()
  
  
