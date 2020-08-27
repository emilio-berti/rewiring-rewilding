library(maxnet) 
library(dismo) 
library(tidyverse)
library(raster)
library(ecospat)
library(doParallel)
library(foreach)

# args <- commandArgs(trailingOnly = T)

setwd("/home/GIT/Trophic_restoration/Code/R")

# k_fold validation function
source("k_fold_function.R")
# function to evalaute the CBI threshold
source("CBI_threshold_function.R")
# SDM function using maxent from maxnet package
source("maxnet_function.R")
# import species alive and extinct names
source("select_species.R")
# prototyping function to get mass of species
source("get_mass.R")

#### NEW CBI - REMOVE FROM RELEASE
source("new_cbi.R")

# import environmental layers
setwd("/NewSpace/Behrmann/v2/wc2.0_5km_bio")
climate <- stack(c("wc2.0_bio_25m_05.tif", "wc2.0_bio_25m_06.tif", "wc2.0_bio_25m_16.tif", "wc2.0_bio_25m_17.tif"))
names(climate) <- c("MaxTemp", "MinTemp", "PrecWet", "PrecDry")

setwd("/NewSpace/Presence_location")

# SDM modeling ------------------------------------------------------------
# set temporary directories
Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
rasterOptions(tmpdir = "/NewSpace/Temp_R/")
#rasterOptions(maxmemory = 150 * 10^6) #set limit to 150 Mb to run on my desktop computer

# remove species without presence location points
modelled <- list.files("/NewSpace/Presence_location/", pattern = "csv") %>% gsub(".csv", "", .)
# remove species with less than 10 points
few_points <- read_csv("../Few_points.txt", col_names = "Species", col_types = cols())$Species
sink("/NewSpace/Maps/Maxent/Log.txt", append = T)
print(paste0("Species without presence locations: ", setdiff(alive, modelled)))
print(paste0("Species with less than 10 presence location points: ", few_points))
sink()

# done <- list.files("../Maps/Maxent/", pattern = "csv") %>% 
#   gsub(".csv", "", .) %>% 
#   gsub("SDM_stat_", "", .) 

alive <- c(alive, "Bos_primigenius", "Camelus_dromedarius", "Oryx_dammah", "Elaphurus_davidianus")
alive <- intersect(alive, modelled)
alive <- setdiff(alive, few_points)

done <- list.files("../Maps/Maxent/", pattern = "csv") %>%
  gsub(".csv", "", .) %>%
  gsub("SDM_stat_", "", .)

alive <- setdiff(alive, done)

# parallel SDMs
registerDoParallel(6)
foreach(x = alive, 
        .packages = c("raster", "dismo", "maxnet", "ecospat", "tidyverse")) %dopar% {
          maxent_sdm(x, map_output = "/NewSpace/Maps/Maxent/")
        } 
stopImplicitCluster()

# if previous SDM_statistics.csv does not work, try this:
# cat SDM_stat* | grep Species | uniq > SDM_statistics.csv
# cat SDM_stat* | grep -v Species >> SDM_statistics.csv
