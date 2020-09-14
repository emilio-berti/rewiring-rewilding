#' DONE
#' This function checks which extant and extinct species present-natural ranges
#' overlap, i.e. which species can be considered reintroduction for rewilding.
#' @param extant is a vector of extant species
#' @param extinct is the extinct species
#' @return vector of @param extant species for reintroduction - empty if
#'   no-reintroducitons are possible.
check_pn_overlap <- function(extant, extinct){
  require(velox)
  pn_extinct <- raster(paste0(pn_path, extinct, ".tif"))
  pn_extinct <- velox(pn_extinct)
  pn_extant <- stack(paste0(pn_path, extant, ".tif"), quick = T)
  pn_extant <- velox(pn_extant)
  overlap <- rep(NA, pn_extant$nbands)
  for(i in 1:pn_extant$nbands){
    overlap[i] <- max(pn_extinct$rasterbands[[1]] * pn_extant$rasterbands[[i]], na.rm = T) == 1
  }
  return(extant[which(overlap == TRUE)])
}
