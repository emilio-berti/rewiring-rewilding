#' This function returns the Biogeographic realms where the extinct species was
#' present.
#' @param extinct species
#' @return names of Bioregions where @param extinct was present
check_bioreg_presence <- function(extinct){
  pn_extinct <- raster(paste0(pn_path, extinct, ".tif"))
  presence <- Bioregions * pn_extinct
  return(names(Bioregions)[which(presence@data@max > 0)])
}
