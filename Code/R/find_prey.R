find_prey <- function(genus, family, level = "Genus"){
  if(level == "Genus"){
    species <- phy %>% 
      filter(Genus.1.2 == genus) %>% 
      pull(Binomial)
  } else if(level == "Family"){
    species <- phy %>% 
      filter(Family.1.2 == family) %>% 
      pull(Binomial)
  }
  
  df <- tibble(`Family` = family, `Genus` = genus, `Pred` = NA, `Prey` = NA)
  
  for(predator in species){
    prey <- get_interactions(predator, interaction.type = "preysOn")$target_taxon_name %>% 
      gsub(" ", "_", .) %>% 
      sort() %>% 
      unique()
    if(length(prey) > 0){
      missing <- which(!prey %in% phy$Binomial.1.2)
      missing_species <- prey[missing]
      
      if(any(missing_species %in% alternative)){
        missing_species[which(missing_species %in% alternative)] <- 
          accepted[which(alternative %in% missing_species)]
      }
      
      prey[missing[which(missing_species %in% phy$Binomial.1.2)]] <- 
        missing_species[which(missing_species %in% phy$Binomial.1.2)]
      
      missing <- which(!prey %in% phy$Binomial.1.2)
      missing_species <- prey[missing]
      
      alternatives <- unlist(lapply(missing_species, function(x){
        y <- name_backbone(x)$species
        if(!is.null(y)){y} else{NA}}))
      
      prey[missing[which(alternatives %in% phy$Binomial)]] <- 
        gsub(" ", "_", alternatives[which(alternatives %in% phy$Binomial)])
      
      missing <- which(!prey %in% phy$Binomial.1.2)
      missing_species <- prey[missing]
      prey <- prey[-missing]
      
      if (length(prey) > 0){
        df <- df %>% 
          add_row(`Family` = family, `Genus` = genus,
                  `Pred` = gsub(" ", "_", predator), `Prey` = prey)
      }
    }
  }
  
  df <- unique(df[-1, ])
  
  return(df)
}
