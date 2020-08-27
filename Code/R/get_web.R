<<<<<<< HEAD
# method can be "QR" for quantile regression (Gravel method) or "ON" for
# optimized niche.

get_web <-  function(species_list, method){
  # import fully connected family-level food web for all mammals
  family_food_web <- read_csv("../../Results/phylogenetic_food_web.csv", col_types = cols())
  # get Phylacine database information
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types= cols()) %>% 
    filter(
      !Family.1.2 %in% marine_families,
      Binomial.1.2 != "Ursus_maritimus",
      Genus.1.2 != "Homo",
      Order.1.2 != "Chiroptera"
    ) %>% 
    transmute(Binomial.1.2, Family.1.2, Mass.g)
  
  family_food_web %>% 
    mutate(Presence = modify2(Pred_family, Prey_family, function(x, y){
      if(phy %>% filter(Family.1.2 %in% x, Binomial.1.2 %in% species_list) %>% nrow() == 0 | 
         phy %>% filter(Family.1.2 %in% y, Binomial.1.2 %in% species_list) %>% nrow() == 0){
        return(F)
      } else(
        return(T)
      )
    })) %>% 
    filter(Presence == T) -> family_food_web
  
  lapply(1:nrow(family_food_web), function(x){
    pred <- phy %>% filter(Family.1.2 == family_food_web$Pred_family[x], Binomial.1.2 %in% species_list) %>% pull(Binomial.1.2)
    prey <- phy %>% filter(Family.1.2 == family_food_web$Prey_family[x], Binomial.1.2 %in% species_list) %>% pull(Binomial.1.2)
    expand.grid(pred, prey) %>%
      as_tibble() %>% 
      mutate(Var1 = as.vector(Var1),
             Var2 = as.vector(Var2)) %>% 
      mutate(`Pred Mass` = modify(Var1, function(y) phy %>% filter(Binomial.1.2 == y) %>% pull(Mass.g) %>% log10()),
             `Prey Mass` = modify(Var2, function(y) phy %>% filter(Binomial.1.2 == y) %>% pull(Mass.g) %>% log10()))
  }) %>% 
    do.call(rbind, .) %>% 
    transmute(Predator = Var1, Prey = Var2, `Pred Mass` = as.numeric(`Pred Mass`), `Prey Mass` = as.numeric(`Prey Mass`)) %>% 
    arrange(Predator) -> phylo_web
  
  if(method == "QR"){
    web <- phylo_web %>% 
      filter(`Prey Mass` < quantile_model$Value[3] + quantile_model$Value[4] * `Pred Mass`,
             `Prey Mass` > quantile_model$Value[1] + quantile_model$Value[2] * `Pred Mass`)
  } else if(method == "ON"){
    web <- phylo_web %>% 
      filter(`Prey Mass` < `Pred Mass` * (niche_model$Value[1] + niche_model$Value[2]),
             `Prey Mass` > `Pred Mass` * (niche_model$Value[1] - niche_model$Value[2]))
  } else{
    error("No method specified.")
  }
  
  return(web)  
  
=======
# method can be "QR" for quantile regression (Gravel method) or "ON" for
# optimized niche.

get_web <-  function(species_list, method){
  # import fully connected family-level food web for all mammals
  family_food_web <- read_csv("../../Results/phylogenetic_food_web.csv", col_types = cols())
  # get Phylacine database information
  phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types= cols()) %>% 
    filter(
      !Family.1.2 %in% marine_families,
      Binomial.1.2 != "Ursus_maritimus",
      Genus.1.2 != "Homo",
      Order.1.2 != "Chiroptera"
    ) %>% 
    transmute(Binomial.1.2, Family.1.2, Mass.g)
  
  family_food_web %>% 
    mutate(Presence = modify2(Pred_family, Prey_family, function(x, y){
      if(phy %>% filter(Family.1.2 %in% x, Binomial.1.2 %in% species_list) %>% nrow() == 0 | 
         phy %>% filter(Family.1.2 %in% y, Binomial.1.2 %in% species_list) %>% nrow() == 0){
        return(F)
      } else(
        return(T)
      )
    })) %>% 
    filter(Presence == T) -> family_food_web
  
  if(nrow(family_food_web) == 0){
    return(0)
  } else{
    lapply(1:nrow(family_food_web), function(x){
      pred <- phy %>% filter(Family.1.2 == family_food_web$Pred_family[x], Binomial.1.2 %in% species_list) %>% pull(Binomial.1.2)
      prey <- phy %>% filter(Family.1.2 == family_food_web$Prey_family[x], Binomial.1.2 %in% species_list) %>% pull(Binomial.1.2)
      expand.grid(pred, prey) %>%
        as_tibble() %>% 
        mutate(Var1 = as.vector(Var1),
               Var2 = as.vector(Var2)) %>% 
        mutate(`Pred Mass` = modify(Var1, function(y) phy %>% filter(Binomial.1.2 == y) %>% pull(Mass.g) %>% log10()),
               `Prey Mass` = modify(Var2, function(y) phy %>% filter(Binomial.1.2 == y) %>% pull(Mass.g) %>% log10()))
    }) %>% 
      do.call(rbind, .) %>% 
      transmute(Predator = Var1, Prey = Var2, `Pred Mass` = as.numeric(`Pred Mass`), `Prey Mass` = as.numeric(`Prey Mass`)) %>% 
      arrange(Predator) -> phylo_web
  }
  if(method == "QR"){
    web <- phylo_web %>% 
      filter(`Prey Mass` < quantile_model$Value[3] + quantile_model$Value[4] * `Pred Mass`,
             `Prey Mass` > quantile_model$Value[1] + quantile_model$Value[2] * `Pred Mass`)
  } else if(method == "ON"){
    web <- phylo_web %>% 
      filter(`Prey Mass` < `Pred Mass` * (niche_model$Value[1] + niche_model$Value[2]),
             `Prey Mass` > `Pred Mass` * (niche_model$Value[1] - niche_model$Value[2]))
  } else{
    error("No method specified.")
  }
  if(nrow(web) == 0){
    return(0)
  } else{
    return(web)  
  }
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
}