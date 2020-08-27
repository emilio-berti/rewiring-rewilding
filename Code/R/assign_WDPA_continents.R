library(tidyverse)
library(sf)

ECO <- read_sf("../../Data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")

WDPA <- st_read("../../Data/PA_5000km.shp")

st_crs(WDPA) <- st_crs(4326)

if(!identical(st_crs(WDPA), st_crs(ECO))){
  stop("CRS not equal")
}
# WDPA <- st_transform(WDPA, st_crs(World))

Bioregions <- ECO %>% 
  group_by(WWF_REALM) %>% 
  summarize(geometry = st_union(geometry)) 

write_sf(Bioregions, "../../Data/Bioregions.shp")