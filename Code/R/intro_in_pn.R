check_intro_in_pn <- function(x, ecozone){
  require(velox)
  ecoraster <- Ecozones[[which(names(Ecozones) == ecozone)]] 
  ecoraster <- velox(ecoraster)
  pn_x <- stack(paste0(pn_path, x, ".tif"))
  pn_x <- velox(pn_x)
  for(i in 1:pn_x$nbands){
    pn_x$rasterbands[[i]] <- pn_x$rasterbands[[i]] * ecoraster$rasterbands[[1]]
  }
  intro_x <- stack(paste0(clim_suit_path, x, "_MSS.tif"))
  intro_x <- velox(intro_x)
  intro_in_pn <- rep(NA, pn_x$nbands)
  for(i in 1:intro_x$nbands){
    in_pn <- rep(NA, pn_x$nbands)
    pn_overlap <- rep(NA, pn_x$nbands)
    for(j in 1:pn_x$nbands){
      if(i != j){
        in_pn[j] <- max(intro_x$rasterbands[[i]] * pn_x$rasterbands[[j]], na.rm = T)
        pn_overlap[j] <- max(pn_x$rasterbands[[i]] * pn_x$rasterbands[[j]], na.rm = T)
      }
    }
    in_pn[which(pn_overlap == 1)] <- NA #remove species that co-occur in present-natural
    intro_in_pn[i] <- sum(in_pn, na.rm = T) > 0
  }
  return(x[which(intro_in_pn == FALSE)])
}

