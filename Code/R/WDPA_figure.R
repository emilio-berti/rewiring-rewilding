library(tmap)
library(sf)
library(tidyverse)

data(World)

setwd("/home/GIT/trophic_restoration/Code/R")

# Ecozones and biomes ----
WWF <- read_sf("/home/emilio/Downloads/official_teow/official/wwf_terr_ecos.shp")

Ecozones <- WWF %>%
  filter(REALM != "AN", !is.na(REALM)) %>% 
  group_by(REALM) %>% 
  summarize()

Biomes <- WWF %>%
  filter(REALM != "AN", !is.na(REALM)) %>% 
  group_by(BIOME) %>% 
  summarize()  

# ecozones figure
Ecozones %>% 
  mutate(Ecozone = REALM) %>% 
  ggplot() +
  geom_sf(aes(fill = Ecozone)) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20)
  ) +
  ggtitle("Ecozones")
ggsave("../../Figures/Ecozones.png")

# biomes figure
Biomes %>% 
  mutate(Biome = factor(BIOME)) %>% 
  ggplot() +
  geom_sf(aes(fill = Biome)) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20)
  ) +
  ggtitle("Biomes")
ggsave("../../Figures/Biomes.png")

write_sf(Ecozones, "../../Data/Ecozone.shp")
write_sf(Biomes, "../../Data/Biomes.shp")

# WDPA ----
wdpa <- read_sf("../Data/WDPA_Feb2019-shapefile/WDPA_Feb2019-shapefile-polygons.shp")

wdpa %>% 
  filter(MARINE == "0") %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II")) %>% 
  filter(GIS_AREA > 5000) %>%
  st_transform(st_crs(World)) %>%
  ggplot() +
  geom_sf(data = World) +
  geom_sf(col = "green4", fill = "green3")

