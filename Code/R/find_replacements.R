find_replacements <- function(extinct, extant){
  # get bioregions where extinct would live
  ecozones <- check_ecozone_presence(extinct)
  gc()
  
  # get species within 0.50-1.50 mass range
  mass <- get_mass(extinct)$Mass
  cand <- get_mass(extant) %>% 
    mutate(`Inside range` = (Mass >= 0.5 * mass & Mass <= 1.5 * mass )) %>% 
    filter(`Inside range` == T) %>% 
    pull(Species)
  
  # restrict for families with many candidates to the genus level to speed up the
  # computations; too many species drains the RAM, and rasterOptions(maxmemory)
  # cannot be set below a Megabyte.
  if((phy %>% 
      filter(Binomial.1.2 == extinct) %>% 
      pull(Family.1.2)) %in% c("Soricidae", "Cricetidae", "Muridae")){
    cand <- phy %>% 
      filter(Genus.1.2 == strsplit(extinct, "_")[[1]][1]) %>% 
      mutate(`Inside range` = (Mass.g >= 0.5 * mass & Mass.g <= 1.5 * mass )) %>% 
      filter(`Inside range` == T) %>% 
      pull(Binomial.1.2)
    cand <- cand[cand %in% clim_suit_modeled]
  }
  cand
  
  # if no species are found and it is an exception check megacategoty
  if(length(cand) == 0 & subset(phy, Binomial.1.2 == extinct)$Order.1.2 == "Proboscidea" & get_mass(extinct)$Mass >= 1000 * 10^3){
    cand <- phy %>% filter(Order.1.2 == "Proboscidea", Mass.g >= 1000 * 10^3, !IUCN.Status.1.2 %in% c("EX", "EP", "EW")) %>% pull(Binomial.1.2)
  } else if(length(cand) == 0 & subset(phy, Binomial.1.2 == extinct)$Family.1.2 == "Felidae" & get_mass(extinct)$Mass >= 100 * 10^3){
    cand <- phy %>% filter(Family.1.2 == "Felidae", Mass.g >= 100 * 10^3, !IUCN.Status.1.2 %in% c("EX", "EP", "EW")) %>% pull(Binomial.1.2)
  } else if(length(cand) == 0 & subset(phy, Binomial.1.2 == extinct)$Family.1.2 == "Ursidae" & get_mass(extinct)$Mass >= 100 * 10^3){
    cand <- phy %>% filter(Binomial.1.2 != "Ursus_maritimus", Family.1.2 == "Ursidae", Mass.g >= 100 * 10^3, !IUCN.Status.1.2 %in% c("EX", "EP", "EW")) %>% pull(Binomial.1.2)
  }
  
  # output for no candidates
  if(length(cand) == 0){
    return(
      tibble(
        Extinct = extinct,
        Ecozone = "All",
        Replacement = "No Replacement",
        Area = 0
      )
    )
  }
  
  # retain only species that overlap in present-natural, if there are any
  pn_overlap <- check_pn_overlap(cand, extinct)
  if(length(pn_overlap) > 0){
    cand <- pn_overlap
  }
  rm(pn_overlap)
  gc()
  
  if(length(cand) > 1){
    tmp_cand <- as.list(rep(NA, length(ecozones)))
    names(tmp_cand) <- ecozones
    for(i in 1:length(tmp_cand)){
      tmp_cand[[i]] <- check_intro_in_pn(cand, names(tmp_cand)[i])
      gc()
    }
  } else if(length(cand) == 1){
    area <- rep(NA, length(ecozones))
    for(i in 1:length(ecozones)){
      area[i] <- get_rewilding_range(cand, extinct, ecozones[i])
      gc()
    }
    res <- tibble(
      Extinct = extinct,
      Ecozone = ecozones,
      Replacement = cand,
      Area = area
    ) %>% 
      filter(!is.na(Extinct))
    return(res)
  }
  gc()
  
  # output
  if(length(cand) > 0){
    # get area of replacements
    res <- tibble("Extinct" = NA, "Ecozone" = NA, "Replacement" = NA, "Area" = NA)
    for(i in 1:length(tmp_cand)){
      ecozone <- names(tmp_cand[i])
      cand <- tmp_cand[[i]]
      area <- sapply(cand, function(x) get_rewilding_range(x, extinct, ecozone))
      res <- res %>% 
        rbind(tibble(
          Extinct = extinct,
          Ecozone = ecozone,
          Replacement = cand,
          Area = area
        )
        ) %>% 
        filter(!is.na(Extinct))
    }
    return(res)
  } else{
    return(
      tibble(
        Extinct = extinct,
        Ecozone = "All",
        Replacement = "No Replacement",
        Area = 0
      )
    )
  }
}