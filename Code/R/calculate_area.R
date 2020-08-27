get_area_pn_bioregion <- function(x, y, bioregion){
  x <- raster(paste0(pn_path, x, ".tif"))
  y <- raster(paste0(clim_suit_path, y, ".tif"))
  area <- area(x * y * Bioregions[[bioregion]])
  return(area)
}