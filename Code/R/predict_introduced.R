library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(foreach)
library(doParallel)

lund_predict <- function(taxon){
  pn <- raster(paste0("/NewSpace/PN_resampled/5km/", taxon, ".tif"))
  r <- raster(paste0("/NewSpace/Maps/Maxent/", taxon, "_MSS.tif"))
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
  pn <- raster(paste0("/NewSpace/PN_resampled/5km/", taxon, ".tif"))
  r <- raster(paste0("/NewSpace/Maps/Maxent/", taxon, "_MSS.tif"))
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


setwd("/home/GIT/Trophic_restoration/Code/R")

source("clean_taxonomy.R")

# Load data from Lundgren -----------------------------------------------------------
lundgren <- read_sf("../../Data/ECOG-03430/IntroducedMegafaunaRangesFinal_%282%29/IntroducedMegafaunaRangesFinal.shp") %>% 
  mutate(Species = gsub("[.]", "_", Species)) %>% 
  mutate(Species = gsub(" ", "_", Species))

species_lund <- lundgren %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

taxonomy <- cbind(species_lund, clean_taxonomy(species_lund)[[1]]) #no missing species for Lundgren dataset

lundgren <- lundgren %>% 
  mutate(Species = map(Species, function(x){
    taxonomy[which(taxonomy[ , 1] == x), 2]
  }) %>% unlist())

species_lund <- lundgren %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

# Load data from IUCN ---------------------------------------------------------------
IUCN <- read_sf("../../Data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp") %>% 
  mutate(Species = gsub(" ", "_", binomial)) %>% 
  filter(origin == 3) #3: introduced; 4: vagrant; 5: uncertain

species_IUCN <- IUCN %>% 
  pull(Species) %>% 
  unique() %>% 
  sort()

taxonomy <- cbind(species_IUCN, clean_taxonomy(species_IUCN)[[1]])
missing <- clean_taxonomy(species_IUCN)[[2]] # no missing
IUCN <- IUCN %>% 
  mutate(Species = map(Species, function(x){
    taxonomy[which(taxonomy[ , 1] == x), 2]
  }) %>% unlist())

species_IUCN <- IUCN %>% 
  pull(Species) %>% 
  unique() %>% 
  setdiff(species_lund) #give priority to Lundgren introduction ranges

modelled <- list.files(paste0("/NewSpace/Maps/Maxent/"), pattern = ".tif") %>% 
  gsub(".tif", "", .)

species_lund <- intersect(species_lund, modelled)
species_IUCN <- intersect(species_IUCN, modelled)

# Predictions -------------------------------------------------------------
# set temporary directories
Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
rasterOptions(tmpdir = "/NewSpace/Temp_R/")
rasterOptions(maxmemory = 150 * 10^6) #set limit to 150 Mb to run on my desktop computer
rasterOptions(tmptime = 2) 
registerDoParallel(6)

foreach(taxon = species_lund,
        .combine = "rbind") %do% {
          lund_predict(taxon)
        } -> Lundgren_proportions

foreach(taxon = species_IUCN,
        .combine = "rbind") %do% {
          IUCN_predict(taxon)
        } -> IUCN_proportions

stopImplicitCluster()

predictions <- tibble(
  Species = c(as.vector(Lundgren_proportions$taxon), as.vector(IUCN_proportions$taxon)),
  Introduction = c(Lundgren_proportions$lund_area, IUCN_proportions$IUCN_area),
  Predicted = c(Lundgren_proportions$lund_predicted, IUCN_proportions$IUCN_predicted),
  Source = c(rep("Lundgren", nrow(Lundgren_proportions)), rep("IUCN", nrow(IUCN_proportions)))
) 

write_csv(predictions, "../../Results/Predictions_introductions.csv", col_names = T)

# Figure ------------------------------------------------------------------
predictions <- read_csv("../../Results/Predictions_introductions.csv", col_names = T, col_types = cols())

pred <- predictions %>% 
  ggplot() +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed") +
  geom_point(aes(Introduction, Predicted, col = Source), alpha = 0.75) +
  scale_x_log10(limits = c(1, max(predictions$Introduction)), labels = fancy_scientific) +
  scale_y_log10(limits = c(1, max(predictions$Introduction)), labels = fancy_scientific) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    panel.grid = element_line(colour = "gainsboro"),
    legend.position = c(0.05, 0.8),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Introduced range") +
  ylab("Predicted introduced range") +
  scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) +
  ggtitle("") +
  theme(panel.grid = element_blank())

pred

ggsave("../../Manuscript/Figures/intro_predicted.svg", width = 6, height = 5)
ggsave("../../Manuscript/Figures/intro_predicted.png", width = 6, height = 5)

median(predictions$Predicted / predictions$Introduction, na.rm = T)
mad(predictions$Predicted / predictions$Introduction, na.rm = T)
mean(predictions$Predicted / predictions$Introduction, na.rm = T)
sd(predictions$Predicted / predictions$Introduction, na.rm = T)

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