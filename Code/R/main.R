library(maxnet) 
library(dismo) 
library(tidyverse)
library(raster)
library(ecospat)
library(doParallel)
library(foreach)
library(cowplot)

# args <- commandArgs(trailingOnly = T)

setwd("/home/GIT/Trophic_restoration/Code/R")

source("k_fold_function.R")
source("CBI_threshold_function.R")
source("maxnet_function.R")

#import species alive and extinct names
source("select_species.R")
source("get_mass.R")

# SDM modeling ------------------------------------------------------------


# Food web inference ------------------------------------------------------
source("food_web.R")

