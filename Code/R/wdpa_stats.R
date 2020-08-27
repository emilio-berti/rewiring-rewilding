library(tidyverse)
library(sf)
library(skimr)
library(ggsci)

source("monkey_theme.R")

realm <- list(
  "AA" = "Australasia",
  "AT" = "Afrotropic",
  "IM" = "Indomalaya",
  "NA" = "Nearctic",
  "NT" = "Neotropic",
  "PA" = "Palearcitc"
)

CRS <- st_crs(raster::crs(raster::raster("../../Data/PHYLACINE_crs.tif")))

wdpa <- st_read("../../Data/PA_5000km_bioregions.shp") %>% 
  mutate(Area = as.numeric(st_area(geometry)) / 10^6) %>%
  filter(Area > 5000) %>% 
  st_simplify(dTolerance = 100)

web_results <- list.files("../../Results/Webs/", pattern = "csv", full.names = TRUE)
TL <- web_results[grep("TL", web_results)]
TL <- TL[grep("random", TL, invert = T)] #remove random webs
Areas <- sort(as.numeric(gsub('[.]csv', '', gsub(str_split(TL, '[0-9]+', simplify = T)[1], '', TL))))
wdpa <- wdpa[Areas, ]

s <- wdpa %>% 
  group_by(Bioregion) %>% 
  skim()

wdpa %>% 
  group_by(Bioregion) %>% 
  mutate(lab_pos = max(Area)) %>% 
  add_tally() %>% 
  ggplot() +
  geom_violin(aes(Bioregion, Area, fill = Bioregion), show.legend = FALSE) +
  geom_jitter(aes(Bioregion, Area), width = 0.20, alpha = 0.5) +
  geom_text(aes(Bioregion, lab_pos + 5000, label = paste0("n = ", n))) +
  xlab("Biogeographic realm") +
  ylab(expression("Area (km"^2*")")) +
  scale_y_continuous(breaks = seq(0, 100000, length.out = 5), labels = monkey_scientific) +
  scale_x_discrete(labels = realm) +
  theme_classic()

ggsave("../../Manuscript/Figures/wdpa_stats.png", width = 12, height = 8, units = "cm")

# test area differences
test <- s %>% 
  as_tibble() %>% 
  filter(variable == "Area")
  
kruskal.test(test$value, test$Bioregion)

# test sample differences
test <- wdpa %>% 
  group_by(Bioregion) %>% 
  add_tally()

kruskal.test(test$n, test$Bioregion)
kruskal.test(test$Area, test$Bioregion)

# random areas -----
ra <- st_read("../../Data/Random_points.shp") %>% 
  mutate(Area = 5024) 

ra %>% 
  group_by(REALM) %>% 
  mutate(lab_pos = max(Area)) %>% 
  add_tally() %>% 
  ggplot() +
  geom_violin(aes(REALM, Area, fill = REALM), show.legend = FALSE) +
  geom_jitter(aes(REALM, Area), width = 0.20, alpha = 0.5) +
  geom_text(aes(REALM, lab_pos + 5, label = paste0("n = ", n))) +
  xlab("Biogeographic realm") +
  ylab(expression("Area (km"^2*")")) +
  scale_y_continuous(breaks = seq(0, 6000, length.out = 5), labels = monkey_scientific) +
  scale_fill_jama(labels = realm) +
  scale_x_discrete(labels = realm) +
  theme_classic()

TL <- web_results[grep("TL", web_results)]
TL <- TL[grep("random", TL)] #get random webs
Areas <- sort(as.numeric(gsub('[.]csv', '', gsub(str_split(TL, '[0-9]+', simplify = T)[1], '', TL))))
plot(st_transform(World$geometry, st_crs(ra)))
plot(ra[setdiff(1:216, Areas), "geometry"], cex = 10, add = T)
