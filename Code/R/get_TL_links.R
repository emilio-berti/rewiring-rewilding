<<<<<<< HEAD
get_TL_links <- function(fw){
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
    transmute(Mass.g, Binomial.1.2, `Meat eater` = Diet.Vertebrate > 0)
  
  species <- append(fw$Predator, fw$Prey) %>% unique()
  
  TL <- trophic_levels(species)
  if(class(TL) == "numeric"){
    return(0)
  }
  
  TL_links <- fw %>% 
    mutate(
      `Pred class` = map(Predator, function(x){
        filter(TL$species_class, Binomial.1.2 == x)$Size
      }) %>% unlist() %>% factor(levels = c("Microherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")),
      `Prey class` = map(Prey, function(x){
        filter(TL$species_class, Binomial.1.2 == x)$Size
      }) %>% unlist() %>% factor(levels = c("Microherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore"))
    ) %>% 
    dplyr::select(`Pred class`, `Prey class`) %>% 
    transmute(
      `Pred edge` = map(`Pred class`, function(x) which(levels(x) == x)) %>% unlist(),
      `Prey edge` = map(`Prey class`, function(x) which(levels(x) == x)) %>% unlist()
    )
  
  # Graph ----
  g <- graph(t(TL_links[, 2:1])) %>% 
    set_edge_attr("weight", value = 1) %>% 
    simplify(edge.attr.comb = list(weight="sum", "ignore"))
  
  return(g)
=======
get_TL_links <- function(fw){
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
    transmute(Mass.g, Binomial.1.2, `Meat eater` = Diet.Vertebrate > 0)
  
  species <- append(fw$Predator, fw$Prey) %>% unique()
  
  TL <- trophic_levels(species)
  if(class(TL) == "numeric"){
    return(0)
  }
  
  TL_links <- fw %>% 
    mutate(
      `Pred class` = map(Predator, function(x){
        filter(TL$species_class, Binomial.1.2 == x)$Size
      }) %>% unlist() %>% factor(levels = c("Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore")),
      `Prey class` = map(Prey, function(x){
        filter(TL$species_class, Binomial.1.2 == x)$Size
      }) %>% unlist() %>% factor(levels = c("Microherbivore", "Mesoherbivore", "Megaherbivore", "Microcarnivore", "Mesocarnivore", "Megacarnivore"))
    ) %>% 
    dplyr::select(`Pred class`, `Prey class`) %>% 
    transmute(
      `Pred edge` = map(`Pred class`, function(x) which(levels(x) == x)) %>% unlist(),
      `Prey edge` = map(`Prey class`, function(x) which(levels(x) == x)) %>% unlist()
    )
  
  # Graph ----
  g <- graph(t(TL_links[, 2:1])) %>% 
    set_edge_attr("weight", value = 1) %>% 
    simplify(edge.attr.comb = list(weight="sum", "ignore"))
  
  return(g)
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
}