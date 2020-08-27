library(tidyverse)
library(sf)
library(tmap)
library(cowplot)
library(grid)
library(gridExtra)

source("my_theme.R")

#get areas with mammals and links in the present natural
web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
random <- grep("random", web_results)
TL <- web_results[grep("TL", web_results)]
TL_random <- TL[grep("random", TL, invert = F)] #only random webs
TL <- TL[grep("random", TL, invert = T)] #remove random webs
Areas <- sort(as.numeric(gsub('[.]csv', '', gsub(str_split(TL, '[0-9]+', simplify = T)[1], '', TL))))
Random <- sort(as.numeric(gsub('[.]csv', '', gsub(str_split(TL_random, '[0-9]+', simplify = T)[1], '', TL_random))))

wdpa <- read_sf("../../Data/PA_5000km_bioregions.shp") %>%
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000) %>% 
  st_simplify(dTolerance = 100)
wdpa <- wdpa[Areas, ]

rand_area <- read_sf("../../Data/Random_points.shp") %>% 
  st_simplify(dTolerance = 100)
rand_area <- rand_area[Random, ]

temp <- read_sf("../../Data/Bioregions.shp") %>% 
  st_simplify(dTolerance = 100)

data(World)

Ecozones <- read_sf('../../Data/Ecozone.shp') %>% 
  st_transform(st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")) %>% 
  st_simplify(dTolerance = 1000)

p1 <- ggplot() +
  geom_sf(data = Ecozones, fill = "white", size = 0.25) +
  geom_sf(data = wdpa, aes(fill = Bioregion, col = Bioregion)) +
  coord_sf(xlim = c(-10800000, 15000000), ylim = c(-6020148, 8751439)) +
  ggtitle("Protected areas") +
  monkey_map

p2 <- ggplot() +
  geom_sf(data = Ecozones, fill = "white", size = 0.25) +
  geom_sf(data = rand_area, aes(fill = REALM, col = REALM), size = 1) +
  coord_sf(xlim = c(-10800000, 15000000), ylim = c(-6020148, 8751439)) +
  ggtitle("Random areas") +
  monkey_map

legend <- wdpa %>% 
  mutate(Bioregion = modify(Bioregion, function(x){
    if(x == "AT"){
      "Afrotropic"
    } else if(x == "AA"){
      "Australasia"
    } else if(x == "NT"){
      "Neotropic"
    } else if(x == "PA"){
      "Palearctic"
    } else if(x == "NA"){
      "Nearctic"
    } else if(x == "IM"){
      "Indomalaya"
    }
  })) %>% 
  mutate(Bioregion = factor(Bioregion, levels = c("Australasia", 
                                                  "Afrotropic", 
                                                  "Indomalaya", 
                                                  "Nearctic", 
                                                  "Neotropic", 
                                                  "Palearctic"))) %>% 
  ggplot() +
  geom_sf(aes(fill = Bioregion)) +
  scale_fill_discrete("Biogeographic realm") +
  theme(legend.position = "right", legend.justification = "center")

legend <- get_legend(legend)

ggdraw(
  plot_grid(
    plot_grid(p1, p2, ncol = 1, labels = "auto"),
    plot_grid(NULL, legend, ncol = 1),
    rel_widths = c(1, 0.3)
  )
)

ggsave("../../Manuscript/Figures/locations.png", width = 8, height = 6)

Ecozones <- Ecozones %>% 
  filter(REALM != 'OC') %>% 
  mutate(Ecozone = map(REALM, function(x){
    switch (x,
            "AT" = "Afrotropic",
            "AA" = "Australasia",
            "NT" = "Neotropic",
            "PA" = "Paleartic",
            "NA" = "Neartic",
            "IM" = "Indomalaya"
    )}) %>% unlist())

ggplot() +
  geom_sf(data = Ecozones, aes(fill = Ecozone)) +
  theme_map()
