#' Get the climatic suitable area of species @param x in the present-natural
#' range of species @param y within a biogeographic realm (aka bioregion).
#' Dependency of replacements.R
get_rewilding_range <- function(x, y, bioregion){
  intro <- raster(paste0(clim_suit_path, x, "_MSS.tif"))
  pn_extant <- raster(paste0(pn_path, x, ".tif"))
  re_intro <- intro + pn_extant
  pn <- raster(paste0(pn_path, y, ".tif"))
  range <- re_intro * pn * Bioregions[[bioregion]]
  range[range > 0] <- 1
  area_in_pn <- sum(values(range), na.rm = T) * 25 #at 5 km resolution
  return(area_in_pn)
}
