# This script exclude all orders whose species are all extinct 
exclude <- function(x){
  iucn_status <- phy %>% 
    filter(Genus.1.2 == filter(phy, Binomial.1.2 == x)$Genus.1.2) %>% 
    pull(IUCN.Status.1.2) %>% 
    as.vector() %>% 
    unique()
  if(all(iucn_status %in% c("EX", "EW", "EP"))){
    return(1)
  } else{
    return(0)
  }
}

exclude_genus <- phy %>%
  pull(Binomial.1.2) %>% 
  map(function(x) exclude(x)) %>% 
  unlist()

exclude_genus <- phy[which(exclude_genus == 1), ] %>% 
  pull(Genus.1.2) %>% 
  unique()

phy <- filter(phy, !Genus.1.2 %in% exclude_genus)

rm(exclude_genus)