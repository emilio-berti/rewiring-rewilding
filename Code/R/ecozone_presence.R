check_ecozone_presence <- function(extinct){
  pn_extinct <- raster(paste0(pn_path, extinct, ".tif"))
  presence <- Ecozones * pn_extinct
  return(names(Ecozones)[which(presence@data@max > 0)])
}
