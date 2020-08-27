infer_pn <- function(protected_area, pn_stack){
  # protected_area <- st_simplify(protected_area, dTolerance = 1000) #does not reduce a lot computation time
  # This takes around 10 minutes
  registerDoParallel(6)
  foreach(x = names(pn_stack)) %dopar% {
    intersect(pn_stack[[x]], protected_area) # intersect and crop take the same time; intersect fits more our purpose
  } %>% 
    brick() -> current_in_area
  stopImplicitCluster()
  # get species with at least one intersecting cell
  species_present <- names(current_in_area)[which(current_in_area@data@max == 1)]
  return(species_present)
}