# This script select alive species to model and extinct species for which we are
# looking for replacements: 'alive' and 'extinct'

# Select all species from marine families
marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")

alive <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
  filter(
    !Family.1.2 %in% marine_families, #excluding cetaceans and pinnipeds families
    Order.1.2 != "Chiroptera", #excluding bats
    !IUCN.Status.1.2 %in% c("EW", "EX", "EP"), #excluding extinct species
    Binomial.1.2 != "Ursus_maritimus" #excluding polar bear
  ) %>% 
  pull(Binomial.1.2)

extinct <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
  filter(
    !Family.1.2 %in% marine_families, #excluding cetaceans and pinnipeds families
    Order.1.2 != "Chiroptera", #excluding bats
    IUCN.Status.1.2 %in% c("EW", "EX", "EP"), #excluding extinct species
    Binomial.1.2 != "Ursus_maritimus" #excluding polar bear
  ) %>% 
  pull(Binomial.1.2)

rm(marine_families)
