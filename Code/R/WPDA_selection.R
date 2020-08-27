library(tidyverse)
library(sf)
library(tmap)
library(foreach)
library(doParallel)

setwd("/home/GIT/Trophic_restoration/Code/R")

# First WDPA cleaning -----------------------------------------------------
# Import WDPA, filter to retain only non marine IUCN categories Ia, Ib, and II, and write
# it to a file for future imports.
WDPA <- read_sf("../../Data/WDPA_Feb2019-shapefile/WDPA_Jun2019-shapefile-polygons.shp") %>% 
  filter(
    MARINE == 0,
    IUCN_CAT %in% c("Ia", "Ib", "II"),
    STATUS %in% c("Designated", "Inscribed", "Established"),
    REP_AREA > 0,
    DESIG_ENG != "UNESCO-MAB Biosphere Reserve"
  )

write_sf(WDPA, "../../Data/WDPA_Cat_I_II.shp")

# Second WDPA cleaning ----------------------------------------------------
# library(wdpar) # returns an error and it is difficult to debug.
# I proceeded as follow:
# 1. I get all intersecting geometries
# 2. I get all protected areas that are connected through 
#    an intermediate geometry
# 3. I modify the st_intersect() object from step 1
# 4. I retain only unique interconnected sets
# 5. 

CRS <- st_crs(raster::crs(raster::raster("/NewSpace/Maps/500km/Abditomys_latidens_MSS.tif")))

WDPA <- read_sf("../../Data/WDPA_Cat_I_II.shp")
st_crs(WDPA) <- st_crs(4326)
WDPA %>% 
  st_transform(CRS) %>% 
  st_simplify(dTolerance = 100) #I found this to be a good trade-off between accuracy and object size

# get intersecting geometries
geom_intersections <- st_intersects(WDPA)

# assign intersections to geometries interconnected via another area 
temp_inters <- geom_intersections
for(i in 1:length(geom_intersections)){
  if(length(geom_intersections[[i]]) > 1){
    for(j in geom_intersections[[i]]){
      geom_intersections[[i]] <- c(geom_intersections[[i]], geom_intersections[[j]])
      geom_intersections[[i]] <- sort(unique(geom_intersections[[i]]))
    }
  }
}

while(!identical(temp_inters, geom_intersections)){
  temp_inters <- geom_intersections
  for(i in 1:length(geom_intersections)){
    if(length(geom_intersections[[i]]) > 1){
      for(j in geom_intersections[[i]]){
        geom_intersections[[i]] <- c(geom_intersections[[i]], geom_intersections[[j]])
        geom_intersections[[i]] <- sort(unique(geom_intersections[[i]]))
      }
    }
  }
}

# remove duplicates
for(x in geom_intersections){
  if(length(x) > 1){
    for(i in x[2:length(x)]){
      geom_intersections[i] <- NA
    }
  } 
}

to_dissolve <- which(!is.na(geom_intersections))

message(paste0(nrow(WDPA) - length(to_dissolve), " protected areas removed into dissolve."))

# dissolve geometries
T0 <- Sys.time()
registerDoParallel(6)
foreach(i = to_dissolve) %dopar% {
  st_union(WDPA[geom_intersections[[i]], ])
} -> WDPA_dissolved
stopImplicitCluster()
Sys.time() - T0

# get total area of dissolved geometries
WDPA_area <- sapply(WDPA_dissolved, function(x) st_area(x))

# retain only protected areas larger than 5000 km2
large_WDPA <- which(WDPA_area / 10^4 > 5000)

wdpa <- tibble(
  ID = 1:length(WDPA_dissolved),
  Extent = map(WDPA_dissolved, function(x) raster::extent(as_Spatial(x))) %>% unlist()
  ) %>% 
  filter(ID %in% large_WDPA)

wdpa$geometry <- rep(NA, nrow(wdpa))
for(i in 1:nrow(wdpa)){
  wdpa$geometry[i] <- st_geometry(WDPA_dissolved[[large_WDPA[i]]])
}

WDPA <- st_as_sf(wdpa)

write_sf(WDPA, "../../Data/PA_5000km.shp")