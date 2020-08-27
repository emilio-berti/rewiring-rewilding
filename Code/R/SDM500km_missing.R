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

# import environmental layers
setwd("/NewSpace/Behrmann/v1")
climate <- stack(c("bio5.tif", "bio6.tif", "bio16.tif", "bio17.tif"))
names(climate) <- c("MaxTemp", "MinTemp", "PrecWet", "PrecDry")

setwd("/NewSpace/Presence_location")

# SDM modeling ------------------------------------------------------------
# set temporary directories
Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
rasterOptions(tmpdir = "/NewSpace/Temp_R/")
rasterOptions(maxmemory = 150 * 10^6) #set limit to 150 Mb to run on my desktop computer
rasterOptions(tmptime = 2) #set time limit to 1 hour to avoid filling disk

# remove species without presence location points
modelled <- list.files("/NewSpace/Presence_location/", pattern = "csv") %>% gsub(".csv", "", .)
# remove species with less than 10 points
few_points <- read_csv("../Few_points.txt", col_names = "Species", col_types = cols())$Species
sink("/NewSpace/Maps/500km/Log.txt", append = T)
print(paste0("Species without presence locations: ", setdiff(alive, modelled)))
print(paste0("Species with less than 10 presence location points: ", few_points))
sink()

alive <- intersect(setdiff(alive, few_points), modelled)

modelled <- gsub(".pdf", "", list.files("/NewSpace/Maps/500km/", pattern = "pdf"))

missing <- setdiff(alive, modelled)

# parallel SDMs
T0 <- Sys.time()
registerDoParallel(6)
foreach(x = missing, 
        .packages = c("raster", "dismo", "maxnet", "ecospat", "tidyverse")) %dopar% {
            maxent_sdm(x, buffer = 500, map_output = "/NewSpace/Maps/500km/")
        } -> sdm_stats
stopImplicitCluster()

# write csv of results
sdm_stats %>% 
  do.call(rbind, .) %>% 
  filter(Species != "0") %>% # remove failing SDMs
  write_csv("/NewSpace/Maps/500km/SDM_statistics.csv",
            append = F,
            col_names = T)
Sys.time() - T0

# if previous SDM_statistics.csv does not work, try this:
# cat SDM_stat* | grep Species | uniq > SDM_statistics.csv
# cat SDM_stat* | grep -v Species >> SDM_statistics.csv
