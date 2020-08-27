get_area_pn_ecozone <- function(extant, extinct, ecozone){
  ecoraster <- Ecozones[[which(names(Ecozones) == ecozone)]] 
  intro <- raster(paste0(clim_suit_path, extant, "_MSS.tif"))
  pn_extant <- raster(paste0(pn_path, extant, ".tif"))
  re_intro <- intro + pn_extant
  pn <- raster(paste0(pn_path, extinct, ".tif"))
  range <- re_intro * pn * ecoraster
  range[range > 0] <- 1
  area_in_pn <- sum(values(range), na.rm = T) * 25 #at 5 km resolution
  return(area_in_pn)
}
