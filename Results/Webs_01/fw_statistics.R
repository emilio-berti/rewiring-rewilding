number_species_level <- function(g){
  MD <- read_tsv("../../Data/MammalDIET.1.0.txt", col_types = cols()) %>% 
    filter(Plant == 1) %>% 
    transmute(Species = paste(Genus, Species, sep = "_"), Woody, Herbaceous)
  
  carnivore_n <- g %>% 
    mutate(`TL` = modify(Predator, function(x){
      if(x %in% g$Prey){
        return("Meso")
      } else{
        return("Apex")
      }
    })) %>% 
    group_by(TL, Scenario) %>% 
    dplyr::select(Predator, TL, Scenario) %>% 
    unique() %>% 
    tally() %>%
    spread(key = TL, value = n)
  
  herbivore_n <- g %>% 
    filter(Prey %in% MD$Species) %>% 
    mutate(`TL` = modify(Prey, function(x){
      if(MD %>% filter(Species == x) %>% pull(Woody)){
        return("Browser")
      } else if(MD %>% filter(Species == x) %>% pull(Herbaceous)){
        return("Grazer")
      } else{
        return("Other")
      }
    })) %>% 
    group_by(TL, Scenario) %>% 
    dplyr::select(Prey, TL, Scenario) %>% 
    unique() %>% 
    tally() %>%
    spread(key = TL, value = n)
}