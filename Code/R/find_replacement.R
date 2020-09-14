#' DELETE ?
find_replacements <- function(extinct, candidates, family){
  bioregions <- check_bioreg_presence(extinct)
  if(nrow(candidates %>% filter(Mass >= 0.5 * extinct$Mass, Mass <= 1.5 * extinct$Mass)) == 0){
    if(extinct$Mass > 1000000 & family %in% c("Gomphotheriidae", "Elephantidae", "Mammutidae", "Stegodontidae")){
      cand <- candidates %>% 
        filter(Mass > 1000000)
    } else if(family %in% c("Ursidae", "Felidae") & extinct$Mass >= 100000){
      cand <- candidates %>% 
        filter(Mass > 100000)
    }
  } else{
    cand <- candidates %>% 
      filter(Mass >= 0.5 * extinct$Mass, Mass <= 1.5 * extinct$Mass) %>% 
      pull(Species)
  }
  if(length(cand) == 0){
    return(
      tibble(
        Extinct = extinct$Species,
        Bioregion = "All",
        Replacement = "No Replacement",
        `Area in Present Natural in Bioregion` = 0
      )
    )
  } else if(length(cand) == 1){
    area <- sapply(bioregions, function(x) get_rewilding_range(cand, extinct$Species, x))
    return(
      tibble(
        Extinct = extinct$Species,
        Bioregion = bioregions,
        Replacement = cand,
        `Area in Present Natural in Bioregion` = area
      )
    )
  } else{
    # check which alive species overlapped in PN with extinct species
    dead_alive_co_occured <- sapply(cand, check_pn_overlap, extinct$Species)
    ### retain only species that co-occurred with extinct if there are any
    ### cand <- names(dead_alive_co_occured)[which(dead_alive_co_occured) == T]
    # if only one extant species overlapped in PN with extinct species, then chose it as replacement
    if(sum(dead_alive_co_occured) == 1){
      if(any(dead_alive_co_occured)){
        cand <- cand[which(dead_alive_co_occured)]
      }
      area <- sapply(bioregions, function(x) get_rewilding_range(cand, extinct$Species, x))
      return(
        tibble(
          Extinct = extinct$Species,
          Bioregion = names(area),
          Replacement = cand,
          `Area in Present Natural in Bioregion` = area
        )
      )
    } else{ 
      # check if introduction is in another extant species present-natural
      intro_in_alive_pn <- mapply(function(x, z) check_intro_in_pn(x, cand, z),
                                  rep(cand, length(bioregions)),
                                  rep(bioregions, each = length(bioregions)), # third argument
                                  SIMPLIFY = T)
      # simplify the mapply output
      intro_in_alive_pn <- sapply(cand, function(x){
        any(intro_in_alive_pn[which(names(intro_in_alive_pn) == x)])
      })
      # retain only the species without introduction in a present-natural range
      # of another living candidate if they did not co-occur or candidates that
      # co-occurred with the extinct species
      cand <- which(intro_in_alive_pn == F | dead_alive_co_occured == T) %>% 
        names()
      if(length(cand) > 0){
        # get area of replacements
        area <- mapply(function(x, z) get_rewilding_range(x, extinct$Species, z),
                       rep(cand, length(bioregions)),
                       rep(bioregions, each = length(bioregions)),
                       SIMPLIFY = T)
        return(
          tibble(
            Extinct = extinct$Species,
            Bioregion = rep(bioregions, each = length(bioregions)),
            Replacement = names(area),
            `Area in Present Natural in Bioregion` = area
          )
        )
      } else{
        return(
          tibble(
            Extinct = extinct$Species,
            Bioregion = "All",
            Replacement = "No Replacement",
            `Area in Present Natural in Bioregion` = 0
          )
        )
      }
    }
  }
}
