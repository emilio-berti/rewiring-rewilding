MD <- read_tsv("/home/GIT/Trophic_restoration/Data/MammalDIET.1.0.txt") %>% 
  mutate(Species = paste(Genus, Species, sep = "_"))

marine_families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")

phy <- read_csv("/home/GIT/Megalinkers/Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv")

phyl <- read_csv("/home/GIT/Megalinkers/Data/PHYLACINE_1.2.0/Data/Taxonomy/Synonymy_table_valid_species_only.csv") %>% 
  mutate(Alt1 = paste(Genus.1.1, Species.1.1, sep = "_"),
         Alt0 = paste(Genus.1.0, Species.1.0, sep = "_"),
         Alt2 = paste(EltonTraits.1.0.Genus, EltonTraits.1.0.Species, sep = "_"))

phy <- phy %>% 
  mutate(Alt0 = phyl$Alt0,
         Alt1 = phyl$Alt1,
         Alt2 = phyl$Alt2) %>% 
  filter(!Family.1.2 %in% marine_families) %>% 
  filter(Order.1.2 != "Chiroptera")

phy <- phy %>% 
  filter(Diet.Plant >= 90) %>% 
  select(Binomial.1.2, Genus.1.2, Alt1, Alt2, IUCN.Status.1.2)

missing <- phy %>% 
  filter(!Binomial.1.2 %in% MD$Species & !Alt1 %in% MD$Species & !Alt2 %in% MD$Species) %>% 
  select(Binomial.1.2, Genus.1.2, IUCN.Status.1.2)

H <- MD %>% 
  filter(TrophicLevel == "Herbivore") %>% 
  select(Species, Genus, Woody, Herbaceous) %>% 
  filter(Woody == 1 | Herbaceous == 1)

missing %>% 
  filter(!Genus.1.2 %in% H$Genus)

