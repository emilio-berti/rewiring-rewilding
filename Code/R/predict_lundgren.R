library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(foreach)
library(doParallel)

lund_predict <- function(taxon){
  pn <- raster(paste0("/home/emilio/PN_resampled/5km/", taxon, ".tif"))
  r <- raster(paste0("/home/emilio/Buffer_500km/", taxon, ".tif"))
  lund <- lundgren %>% 
    filter(Species == taxon) %>% 
    st_transform(st_crs(r)) %>% 
    fasterize(r)
  if(identical(lund, mask(lund, pn, maskvalue = 0))){
    return(NA)
  } else{
    re_intro <- pn + r
    re_intro[re_intro > 1] <- 1
    lund_area <- sum(values(lund), na.rm = T)
    lund_predicted <- sum(values(lund * re_intro), na.rm = T)
    return(data.frame(taxon, lund_predicted, lund_area))
  }
}

IUCN_predict <- function(taxon){
  pn <- raster(paste0("/home/emilio/PN_resampled/5km/", taxon, ".tif"))
  r <- raster(paste0("/home/emilio/Buffer_500km/", taxon, ".tif"))
  
  IUCN_raster <- IUCN %>% 
    filter(Species == taxon) %>% 
    st_transform(st_crs(r)) %>% 
    fasterize(r)
  
  if(identical(IUCN_raster, mask(IUCN_raster, pn, maskvalue = 0))){
    return(NA)
  } else{
    re_intro <- pn + r
    re_intro[re_intro > 1] <- 1
    IUCN_area <- sum(values(IUCN_raster), na.rm = T)
    IUCN_predicted <- sum(values(IUCN_raster * re_intro), na.rm = T)
    return(data.frame(taxon, IUCN_predicted, IUCN_area))
  }
}

setwd("/home/GIT/trophic_restoration/Code/R")

source("clean_taxonomy.R")

lundgren <- read_sf("../../Data/ECOG-03430/IntroducedMegafaunaRangesFinal_%282%29/IntroducedMegafaunaRangesFinal.shp") %>% 
  mutate(Species = gsub("[.]", "_", Species)) %>% 
  mutate(Species = gsub(" ", "_", Species))

species <- lundgren %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

taxonomy <- cbind(species, clean_taxonomy(species)[[1]]) #no missing species for Lundgren dataset

lundgren <- lundgren %>% 
  mutate(Species = map(Species, function(x){
    taxonomy[which(taxonomy[ , 1] == x), 2]
  }) %>% unlist())


# Predictions -------------------------------------------------------------



lundgren_species <- lundgren %>% 
  pull(Species) %>% 
  unique()

modelled <- list.files("/home/emilio/Buffer_500km", pattern = ".tif") %>% 
  gsub(".tif", "", .)

species <- intersect(lundgren_species, modelled)

registerDoParallel(6)

foreach(taxon = species,
        .combine = "rbind") %dopar% {
          lund_predict(taxon)
        } -> Lundgren_proportions

stopImplicitCluster()

write_csv(Lundgren_proportions, "../../Results/Lundgren_500km.csv")

Lundgren_proportions %>% 
  mutate(lund_predicted = sapply(lund_predicted, function(x){00
    if(x == 0){return(1)} else{return(x)}
  })) %>% 
  ggplot() +
  geom_point(aes(lund_area, lund_predicted)) +
  scale_x_log10(limits = c(1, max(Lundgren_proportions$lund_area))) +
  scale_y_log10(limits = c(1, max(Lundgren_proportions$lund_area))) +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gainsboro")
  ) +
  xlab("Introduced megafauna range")

# Figure ------------------------------------------------------------------
# library(tmap)
# data("World")
# 
# lundgren %>% 
#   ggplot() +
#   geom_sf(data = World %>% st_transform(st_crs(lundgren)), fill = NA) + 
#   geom_sf(aes(fill = Species), show.legend = F) +
#   facet_wrap(Species ~ ., ncol = 5) +
#   theme(
#     panel.background = element_blank()
#   )