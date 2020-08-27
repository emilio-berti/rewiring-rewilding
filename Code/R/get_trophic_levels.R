<<<<<<< HEAD
trophic_levels <- function(x){
  
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
    mutate(`Meat eater` = Diet.Vertebrate > 0)
  
  carnivores <- split(phy, phy$`Meat eater`)$`TRUE`
  herbivores <- split(phy, phy$`Meat eater`)$`FALSE`
  
  if(nrow(filter(carnivores, Binomial.1.2 %in% x)) > 0){
    carn_class <- carnivores %>% 
      filter(Binomial.1.2 %in% x) %>% 
      mutate(Size = map(Mass.g, function(m){
        if(m >= 100 * 10^3){
          "Megacarnivore"
        } else if(m >= 21.5 * 10^3){
          "Mesocarnivore"
        } else{
          "Microcarnivore"
        }
      }) %>% 
        unlist()) %>% 
      dplyr::select(Binomial.1.2, Size)
    
    carn_TL <- carn_class %>% 
      group_by(Size) %>% 
      tally()
  }
  
  if(nrow(filter(herbivores, Binomial.1.2 %in% x)) > 0){
    herb_class <- herbivores %>% 
      filter(Binomial.1.2 %in% x) %>% 
      mutate(Size = map(Mass.g, function(m){
        if(m >= 1000 * 10^3){
          "Megaherbivore"
        } else{
          "Microherbivore"
        }
      }) %>% 
        unlist()) %>% 
      dplyr::select(Binomial.1.2, Size)
    
    herb_TL <- herb_class %>% 
      group_by(Size) %>% 
      tally()
  } else{
    return(0)
  }
  
  if(nrow(filter(carnivores, Binomial.1.2 %in% x)) > 0){
    species_class <- rbind(carn_class, herb_class)
    TL_structure <- rbind(carn_TL, herb_TL)
  } else{
    species_class <- herb_class
    TL_structure <- herb_TL
  }
  
  return(list(species_class = species_class, TL_structure = TL_structure))
}
=======
trophic_levels <- function(x){
  
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
    mutate(`Meat eater` = Diet.Vertebrate > 0 | Genus.1.2 == "Ursus") #bears diet is largely inaccurate in the PHYLACINE database
  
  carnivores <- split(phy, phy$`Meat eater`)$`TRUE`
  herbivores <- split(phy, phy$`Meat eater`)$`FALSE`
  
  if(nrow(filter(carnivores, Binomial.1.2 %in% x)) > 0){
    carn_class <- carnivores %>% 
      filter(Binomial.1.2 %in% x) %>% 
      mutate(Size = map(Mass.g, function(m){
        if(m >= 100 * 10^3){
          "Megacarnivore"
        } else if(m >= 21.5 * 10^3){
          "Mesocarnivore"
        } else{
          "Microcarnivore"
        }
      }) %>% 
        unlist()) %>% 
      dplyr::select(Binomial.1.2, Size)
    
    carn_TL <- carn_class %>% 
      group_by(Size) %>% 
      tally()
  }
  
  if(nrow(filter(herbivores, Binomial.1.2 %in% x)) > 0){
    herb_class <- herbivores %>% 
      filter(Binomial.1.2 %in% x) %>% 
      mutate(Size = map(Mass.g, function(m){
        if(m >= 1000 * 10^3){
          "Megaherbivore"
        } else if(m >= 45 * 10^3){
          "Mesoherbivore"
        } else{
          "Microherbivore"
        }
      }) %>% 
        unlist()) %>% 
      dplyr::select(Binomial.1.2, Size)
    
    herb_TL <- herb_class %>% 
      group_by(Size) %>% 
      tally()
  } else{
    return(0)
  }
  
  if(nrow(filter(carnivores, Binomial.1.2 %in% x)) > 0){
    species_class <- rbind(carn_class, herb_class)
    TL_structure <- rbind(carn_TL, herb_TL)
  } else{
    species_class <- herb_class
    TL_structure <- herb_TL
  }
  
  return(list(species_class = species_class, TL_structure = TL_structure))
}
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
