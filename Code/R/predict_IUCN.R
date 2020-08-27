library(tidyverse)
library(sf)

source("clean_taxonomy.R")

IUCN <- read_sf("../../Data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp") %>% 
  mutate(Species = gsub(" ", "_", binomial)) %>% 
  filter(origin == 3) #3: introduced; 4: vagrant; 5: uncertain

species <- IUCN %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

taxonomy <- cbind(species, clean_taxonomy(species)[[1]])

missing <- clean_taxonomy(species)[[2]] # no missing

IUCN <- IUCN %>% 
  mutate(Species = map(Species, function(x){
    taxonomy[which(taxonomy[ , 1] == x), 2]
  }) %>% unlist())

# Predictions -------------------------------------------------------------
library(raster)
library(fasterize)
library(foreach)
library(doParallel)

iucn_species <- IUCN %>% pull(Species) %>% unique()

modelled <- list.files("/NewSpace/Maps/", pattern = ".tif") %>% 
  gsub(".tif", "", .)

species <- intersect(iucn_species, modelled)

registerDoParallel(6)

foreach(taxon = species,
        .combine = c) %do% {
          
          r <- raster(paste0("/NewSpace/Maps/", taxon, ".tif"))
          
          IUCN_raster <- IUCN %>% 
            filter(Species == taxon) %>% 
            st_transform(st_crs(r)) %>% 
            fasterize(r)
          
          # TESTING: TO WHAT WE COMPARE TO?
          # random_propotion <- sum(lund@data@values, na.rm = T) / sum(values(r), na.rm = T)
          
          threshold <- read_csv("/NewSpace/Maps/SDM_statistics.csv", 
                                col_types = cols(), 
                                col_names = c("Species", "Spearman.CBI", "Dev.Spearman.CBI",
                                              "AUC", "Dev.AUC", "CBI.threshold",
                                              "MSS.threshold", "OR.10", "OR.min", "Dev.OR.min")) %>% 
            filter(Species == taxon) %>% 
            pull(MSS.threshold)
          
          r <- r >= threshold
          
          # plot(r, legend = F)
          # plot(lund, add = T, col = "steelblue", legend = F)
          
          projected_proportion <- sum((IUCN_raster * r)@data@values, na.rm = T) / sum(IUCN_raster@data@values, na.rm = T)
          
          projected_proportion
        } -> IUCN_proporions

stopImplicitCluster()

write_csv(IUCN_proportions, "../../Results/IUCN_predicted_proportions.csv")


# Figure ------------------------------------------------------------------
# IUCN %>% 
#   ggplot() +
#   geom_sf(aes(fill = Species), show.legend = F) + 
#   facet_wrap(Species ~ ., ncol = 10)


