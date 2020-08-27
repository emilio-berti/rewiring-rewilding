megacategory <- function(species){
  mass <- subset(phy, Binomial.1.2 == species)$Mass.g    
  family <- subset(phy, Binomial.1.2 == species)$Family.1.2
  if(family %in% c("Ursidae", "Felidae")){
    candidates <- subset(phy, Family.1.2 == family)[c("Binomial.1.2", "Mass.g", "IUCN.Status.1.2")]
    candidates <- subset(candidates, Mass.g >= 100000)
  } else{
    candidates <- NULL
  }
  if(!is.null(candidates)){
    candidates <- subset(candidates, !IUCN.Status.1.2 %in% c("EX", "EW", "EP"))
    return(as.vector(candidates$Binomial.1.2))
  } else{
    return(NULL)
  }
}