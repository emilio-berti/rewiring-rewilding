library(raster)
library(tidyverse)
library(tmap)
library(sf)
library(fasterize)

source("select_species.R")

repl <- read_csv("../../Results/Replacements.csv", col_types = cols(), col_names = TRUE) %>%  
  filter(Replacement != "No Replacement")
# 
# extinct_families <- phy %>% 
#   filter(Binomial.1.2 %in% extinct) %>% 
#   group_by(Family.1.2) %>% 
#   tally() %>% 
#   arrange(desc(n))
# 
# replaced_families <- phy %>% 
#   filter(Binomial.1.2 %in% repl$Extinct) %>% 
#   group_by(Family.1.2) %>% 
#   tally()
# 
# a <- full_join(extinct_families, replaced_families, by = "Family.1.2") %>% 
#   mutate(Fraction = n.y / n.x) %>% 
#   arrange(Family.1.2)
# 
# svg("../../Manuscript/Figures/Family_replaced.svg", 14, 8)
# par(bg = "gainsboro", mar = c(10, 6, 4, 4))
# graphics::barplot(height = a$n.x, names = a$Family.1.2, xlab = "", ylab = "Number of species", 
#                   col = "tomato", las = 2, main = "")
# graphics::barplot(height = a$n.y, names = a$Family.1.2, add = T, col = "green4", axes = F, cex.names = 0.01)
# legend(x = 60, y = 15, fill = c("tomato", "green4"), legend = c("Extinct", "Replaced"), box.col = NA)
# dev.off()

# rw <- stack(paste0("/NewSpace/Maps/Maxent/", unique(repl$Replacement), "_MSS.tif")) %>%
#   projectRaster(current[[1]], method = "bilinear")
# writeRaster(rw, "../../Data/rewilding.envi", overwrite = T)

r_temp <- raster("../../Data/PHYLACINE_1.2/Data/Ranges/Current/Panthera_leo.tif")

Ecozones <- st_read("../../Data/Ecozone.shp") %>% 
  mutate(Name = c("Australasia", "Afrotropic", "Indomalaya", 
                  "Neartic", "Neotropic", "Oceania", "Paleartic")) %>% 
  st_transform(crs(r_temp)) %>% 
  fasterize(r_temp, by = "Name")

alive <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols()) %>% 
  filter(!IUCN.Status.1.2 %in% c("EX", "EP", "EW")) %>% 
  pull(Binomial.1.2)

replacements <- repl$Replacement
dead <- repl$Extinct
ecozones <- repl$Ecozone

rasterOptions(maxmemory = 10^12)

library(doParallel)
library(foreach)

registerDoParallel(6)
foreach(species = alive) %dopar% {
  pn <- raster(paste0("../../Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", species, ".tif"))
  if(species %in% unique(repl$Replacement)){
    intro <- raster(paste0("/NewSpace/Maps/Maxent/", species, "_MSS.tif")) %>% 
      projectRaster(r_temp)
    dead <- repl %>% filter(Replacement == species) %>% pull(Extinct)
    ecozones <- repl %>% filter(Replacement == species) %>% pull(Ecozone)
    for(i in 1:length(dead)){
      extinct <- dead[i]
      pn_extinct <- raster(paste0("../../Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", extinct, ".tif"))
      ecozone <- Ecozones[[which(names(Ecozones) == ecozones[i])]]
      replacement <- intro * pn_extinct * ecozone
      pn <- sum(pn, replacement, na.rm = T)
      plot(pn, axes = F, main = species)
    } 
    pn[pn > 1] <- 1
  }
  writeRaster(pn, paste0("../../Data/Rewilding_ranges/", species, ".tif"), overwrite = T)
}
stopImplicitCluster()
