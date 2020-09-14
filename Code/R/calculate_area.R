#' DELETE
#' Get the climatic suitable area of species @param y in the present-natural
#' range of species @param x within a biogeographic realm (aka bioregion).
#' Dependency of find_replacement.R
#' OLD, UNSTABLE - use area_intro_in_pn.R instead
get_area_pn_bioregion <- function(x, y, bioregion){
  x <- raster(paste0(pn_path, x, ".tif"))
  y <- raster(paste0(clim_suit_path, y, ".tif"))
  area <- area(x * y * Bioregions[[bioregion]])
  return(area)
}