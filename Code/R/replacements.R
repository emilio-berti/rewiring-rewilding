#' DONE 
#' Author: Emilio Berti
#' e-mail: emilio.berti.academia[@]google.com
#' Updated: 
library(tidyverse)
library(raster)
library(foreach)
library(doParallel)
library(sf)
library(fasterize)
library(velox) #to speed up normal raster calculations

setwd("/home/GIT/Trophic_restoration/Code/R")

source("select_species.R")
source("pn_overlap.R")
source("ecozone_presence.R")
source("intro_in_pn.R")
source("area_intro_in_pn.R")
source("find_replacements.R")
source("get_mass.R")

# set temporary directories
Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
rasterOptions(tmpdir = "/NewSpace/Temp_R/")

phy <- read_csv("../../Data/PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())
pn_path <- "/NewSpace/PN_resampled/5km/"
clim_suit_path <- "/NewSpace/Maps/Maxent/"
res_path <- "../../Results/"

message("SDM suitability ranges from: ", clim_suit_path)
message("Results in: ", res_path)
message("Temporary R rasters in: ", "/NewSpace/Temp_R/")

r_template <- raster(paste0(clim_suit_path, "Acinonyx_jubatus.tif"))
Bioregions <- st_read("../../Data/Ecozone.shp") %>% 
  mutate(Name = c("Australasia", "Afrotropic", "Indomalaya", 
                  "Neartic", "Neotropic", "Oceania", "Paleartic")) %>% 
  st_transform(st_crs(r_template)) %>%
  fasterize(r_template, by = "Name")
# set raster memory limit
rasterOptions(maxmemory = 150 * 10^6)
# read in all families
families <- phy %>% 
  filter(Binomial.1.2 %in% extinct,
         Family.1.2 != "Hominidae") %>%  
  pull(Family.1.2) %>% 
  unique()
done <- list.files("/home/GIT/Trophic_restoration/Results/Replacements", 
                   pattern = ".csv") %>% 
  gsub("_replacements.csv", "", .)
families <- setdiff(families, done)
done <- read_csv("/home/GIT/Trophic_restoration/Results/Replacements_Log.txt", 
                 col_type = cols(),
                 col_names = FALSE) %>% 
  pull(X1) 
done <- str_extract(done, "[A-Z]\\w+") %>% unique() #get names of all taxa that failed
families <- setdiff(families, done)
# count species in each family
number_species_family <- phy %>% 
  filter(Family.1.2 %in% families) %>% 
  group_by(Family.1.2) %>% 
  tally() %>% 
  arrange(n)
# get species for which we have the climatic suitability layer
clim_suit_modeled <- gsub("_MSS.tif", "", 
                          list.files(clim_suit_path, pattern = "_MSS.tif"))
families <- number_species_family$Family.1.2

# Starting algorithm for replacements -------------------------
T0 <- Sys.time()
for (fam in number_species_family$Family.1.2) {
  if (number_species_family$n[which(number_species_family$Family.1.2 == fam)] > 300) {
    # set raster memory limit
    rasterOptions(maxmemory = 10 * 10^6)
  }
  candidates <- phy %>% 
    filter(Family.1.2 == fam, Binomial.1.2 %in% alive) %>% 
    pull(Binomial.1.2)
  if (length(candidates) == 0) {
    sink("../../Results/Replacements_Log.txt", append = T)
    print(paste0(fam, " all extinct."))
    sink()
    next()
  } 
  # remove species that were not modelled
  candidates <- candidates[which(candidates %in% clim_suit_modeled)]
  if (length(candidates) == 0) {
    sink("../../Results/Replacements_Log.txt", append = T)
    print(paste0(fam, " cannot be replaced: missing SDMs for some species."))
    sink()
    next()
  }
  dead <- phy %>% 
    filter(Family.1.2 == fam, Binomial.1.2 %in% extinct) %>% 
    pull(Binomial.1.2)
  if (length(dead) == 0) {
    sink("../../Results/Replacements_Log.txt", append = T)
    print(paste0(fam, " all alive."))
    sink()
    next()
  }
  dead <- phy %>% 
    filter(Family.1.2 == fam, Binomial.1.2 %in% extinct) %>% 
    filter(Island.Endemicity != "Occurs only on isolated islands") %>% 
    pull(Binomial.1.2)
  if (length(dead) == 0) {
    sink("../../Results/Replacements_Log.txt", append = T)
    print(paste0(fam, " extinct species occur only on isolated islands."))
    sink()
    next()
  }
  foreach (i = dead) %do% { #possible to parallelize if enough RAM or disk space
    find_replacements(i, candidates)
  } -> family_results
  # get best replacements
  replacements <- do.call("rbind", family_results) %>% 
    group_by(Extinct, Ecozone) %>% 
    top_n(1) 
  write_csv(replacements, paste0("../../Results/Replacements/", fam, "_replacements.csv"), col_names = T)
  # get non-replaceble extinct species 
  lost <- setdiff(dead, unique(replacements$Extinct))
  if(length(lost) >= 1){
    sink("../../Results/Replacements_Log.txt", append = T)
    print(paste0(lost, " cannot be replaced."))
    sink()
  }
}
Sys.time() - T0


# Proboscidea -------------------------------------------------------------

# Starting function -------------------------------------------------------
T0 <- Sys.time()
order <- "Proboscidea"
candidates <- phy %>% 
  filter(Order.1.2 == order, Binomial.1.2 %in% alive) %>% 
  pull(Binomial.1.2)

dead <- phy %>% 
  filter(Order.1.2 == order, Binomial.1.2 %in% extinct) %>% 
  filter(Island.Endemicity != "Occurs only on isolated islands") %>% 
  pull(Binomial.1.2)

foreach(i = dead) %do% { #possible to parallelize if enough RAM or disk space
  find_replacements(i, candidates)
} -> proboscidea_results

# get best replacements
replacements <- do.call("rbind", proboscidea_results) %>% 
  group_by(Extinct, Ecozone) %>% 
  top_n(1) 
write_csv(replacements, paste0("../../Results/Replacements/", order, "_replacements.csv"), col_names = T)

# get non-replaceble extinct species 
lost <- setdiff(dead, unique(replacements$Extinct))
if(length(lost) >= 1){
  sink("../../Results/Replacements_Log.txt", append = T)
  print(paste0(lost, " cannot be replaced."))
  sink()
}
Sys.time() - T0


