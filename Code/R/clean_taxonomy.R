#this function clear the taxonomy using the PHYLACINE database
clean_taxonomy <- function(species_list){
  if(any(grep(" ", species_list))){
    species_list <- gsub(" ", "_", species_list)
  }
  # create alternative taxonomy
  taxonomy <- read_csv("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2/Data/Taxonomy/Synonymy_table_with_unaccepted_species.csv", col_types = cols()) %>% 
    transmute(
      Binomial.1.2 = paste(Genus.1.2, Species.1.2, sep = "_"),
      Binomial.1.1 = paste(Genus.1.1, Species.1.1, sep = "_"),
      Binomial.1.0 = paste(Genus.1.0, Species.1.0, sep = "_"),
      Binomial.ET = paste(EltonTraits.1.0.Genus, EltonTraits.1.0.Species, sep = "_"),
      Binomial.IUCN = paste(IUCN.2016.3.Genus, IUCN.2016.3.Species, sep = "_")
    )
  #return error message if a species is missing
  if(!is_empty(missing <- species_list[which(!species_list %in% taxonomy$Binomial.1.2 & 
                                             !species_list %in% taxonomy$Binomial.1.1 &
                                             !species_list %in% taxonomy$Binomial.1.0 & 
                                             !species_list %in% taxonomy$Binomial.ET & 
                                             !species_list %in% taxonomy$Binomial.IUCN)])){
    warnings(paste0("Species ", missing, " is missing from taxonomic alternatives."))
  }
  if(!is_empty(missing)){
    to_clean <- species_list[-which(species_list %in% missing)]
  } else{
    to_clean <- species_list
  }
  to_clean <- to_clean[which(!to_clean %in% taxonomy$Binomial.1.2)]
  # cleaned vector
  cleaned <- taxonomy %>% 
    filter(Binomial.1.1 %in% to_clean | Binomial.1.0 %in% to_clean | Binomial.ET %in% to_clean) %>% 
    pull(Binomial.1.2)
  # rebind missing and cleaned
  species_list[which(species_list %in% to_clean)] <- cleaned
  return(list(species_list, missing))
}