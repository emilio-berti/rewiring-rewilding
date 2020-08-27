<<<<<<< HEAD
infer_rewilded <- function(protected_area, rewilded_stack){
  # protected_area <- st_simplify(protected_area, dTolerance = 1000) #does not reduce a lot computation time
  # registerDoParallel(50)
  foreach(x = names(rewilded_stack), .packages = "raster") %dopar% {
    intersect(rewilded_stack[[x]], protected_area) # intersect and crop take the same time; intersect fits more our purpose
  } %>% 
    brick() -> rewilded_in_area
  # stopImplicitCluster()
  # get species with at least one intersecting cell
  species_present <- names(rewilded_in_area)[which(rewilded_in_area@data@max == 1)]
  return(species_present)
=======
infer_rewilded <- function(protected_area, rewilded_stack){
  # protected_area <- st_simplify(protected_area, dTolerance = 1000) #does not reduce a lot computation time
  # registerDoParallel(50)
  foreach(x = names(rewilded_stack), .packages = "raster") %dopar% {
    intersect(rewilded_stack[[x]], protected_area) # intersect and crop take the same time; intersect fits more our purpose
  } %>% 
    brick() -> rewilded_in_area
  # stopImplicitCluster()
  # get species with at least one intersecting cell
  species_present <- names(rewilded_in_area)[which(rewilded_in_area@data@max == 1)]
  return(species_present)
>>>>>>> 12b6b2da9189d1eccc59266d2db8099edab0a4b2
}