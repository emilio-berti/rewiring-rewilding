library(tidyverse)
library(sf)

CRS <- st_crs(raster::crs(raster::raster("../../Data/PHYLACINE_crs.tif")))

# ECO <- read_sf("../../Data/terr-ecoregions-TNC/tnc_terr_ecoregions.shp")
# 
# Bioregions <- ECO %>% 
#   st_transform(CRS) %>% 
#   group_by(WWF_REALM) %>% 
#   summarize(geometry = st_union(geometry)) 
#
# write_sf(Bioregions, "../../Data/Bioregions.shp")

Bioregions <- st_read("../../Data/Bioregions.shp")

WDPA <- st_read("../../Data/PA_5000km.shp")

st_crs(WDPA) <- st_crs(4326)

if(!identical(st_crs(WDPA), st_crs(Bioregions))){
  warning("CRS not equal: reprojecting")
  WDPA <- st_transform(WDPA, st_crs(Bioregions))
}

BIO <- rep(NA, nrow(WDPA))
for(x in 1:nrow(WDPA)){
  if(!st_is_valid(WDPA$geometry[x])){
    WDPA$geometry[x] <- st_buffer(WDPA$geometry[x], 5)
  }
  pos <- rep(F, 8)
  for(y in 1:nrow(Bioregions)){
    cropped <- st_intersection(WDPA$geometry[x], Bioregions$geometry[y])
    pos[y] <- !is_empty(cropped)
  }
  if(sum(pos) == 1){
    BIO[x] <- as.character(Bioregions$WWF_REALM[pos])
  } else{
    warning("The protected area intersects two or more Bioregions:
            only the Bioregion with the largest overlap is assigned to the area.")
    cropped_area <- sapply(which(pos == T), function(z){
      st_intersection(WDPA$geometry[x], Bioregions$geometry[z]) %>% 
        st_area
    })
    if(is_empty(cropped_area)){
      BIO[x] <- "Not Available" #NA is North America
    } else{
      BIO[x] <- as.character(Bioregions$WWF_REALM[pos][which.max(cropped_area)])
    }
  }
}

WDPA <- WDPA %>% 
  mutate(Bioregion = BIO)

write_sf(WDPA, "../../Data/PA_5000km_bioregions.shp")

# Protected area map ------------------------------------------------------
library(tidyverse)
library(sf)
library(tmap)

Bioregions <- st_read("../../Data/Bioregions.shp")

WDPA <- st_read("../../Data/PA_5000km_bioregions.shp") %>% 
  st_simplify(dTolerance = 500) %>% 
  filter(!Bioregion == "Not Available") %>% 
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>% 
  filter(Area > 5000)

WDPA$Bioregion <- droplevels(WDPA$Bioregion)
WDPA$Ecoregion <- WDPA$Bioregion

PA_map <- tm_shape(Bioregions) +
  tm_borders() +
  tm_shape(WDPA) +
  tm_borders(alpha = 0.5, lwd = 0.1) +
  tm_fill(col = "Ecoregion", 
          palette = RColorBrewer::brewer.pal(n = 8, "Set1"))

tmap_save(PA_map, "../../Figures/PA_map.png",
          dpi = 300,
          width = 28, 
          units = "cm")

tmap_save(PA_map, "../../Figures/PA_map.pdf",
          dpi = 300,
          width = 28, 
          units = "cm")

WDPA %>% 
  group_by(Ecoregion) %>% 
  tally()

