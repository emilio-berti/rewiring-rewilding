library(tidyverse)
library(igraph)
library(ggraph)
library(sf)
library(tmap)

setwd("/home/GIT/Trophic_restoration/")

Ecozones <- read_sf('Data/Ecozone.shp') %>% 
  st_transform(54009) %>% 
  st_simplify(dTolerance = 1000)

ggplot() +
  geom_sf(data = Ecozones, aes(fill = REALM), alpha = 0.35) +
  coord_sf(xlim = c(-10800000, 15000000), ylim = c(-6020148, 8751439)) +
  monkey_map

ggsave('Manuscript/Figures/TL_map.png', width = 20, height = 12)

Pal <- rev(c('#00637c', wesanderson::wes_palette('Zissou1')))

Labs <- c("Megacarnivores\n (\u2265 100 kg)",
          "Large carnivores\n (\u2208 [21.5, 100) kg)", 
          "Small carnivores\n (\u2264 21.5 kg)",
          "Megaherbivores\n (\u2265 1,000 kg)",
          "Large herbivores\n (\u2208 [45, 1,000) kg)",
          "Small herbivores\n (\u2264 45 kg)")

raw_levels %>% 
  gather(key = 'Scenario', value = 'n', -Size, -Ecozone, -Web) %>% 
  mutate(
    `Trophic level` = factor(Size, levels = c('Megacarnivore', 'Mesocarnivore', 'Microcarnivore', 'Megaherbivore', 'Mesoherbivore', 'Microherbivore')),
    Scenario = factor(Scenario, levels = c('PN', 'CU', 'RW')),
    Ecozone = map(Ecozone, function(x)
      switch(x,
             'AA' = 'Australasia',
             'AT' = 'Afrotropic',
             'IM' = 'Indomalaya',
             'NA' = 'Neartic',
             'NT' = 'Neotropic',
             'PA' = 'Paleartic'
      )
    ) %>% unlist()
  ) %>% 
  ggplot() +
  geom_boxplot(aes(Scenario, n, fill = Size), outlier.shape = NA) +
  geom_smooth(aes(Scenario, n, col = Size, group = Size), alpha = 0.2) +
  scale_fill_manual(values = Pal, labels = Labs) +
  scale_color_manual(values = Pal) +
  scale_y_sqrt(breaks = c(1, 10, 23, 40, 61), limits = c(0, 70)) +
  facet_rep_wrap(Ecozone ~ ., repeat.tick.labels = TRUE) +
  ylab('Number of species') +
  monkey_theme +
  theme(text = element_text(size = 40))

Pal <- wesanderson::wes_palette('Darjeeling1', type = 'continuous', n = 11)

raw_edges %>% 
  group_by(From, To) %>% 
  gather(key = 'Scenario', value = 'n', -i, -j, -Ecozone, -Web, -From, -To) %>% 
  mutate(Size = paste0(From, ' \u2192 ', To)) %>% 
  ggplot() +
  geom_boxplot(aes(Scenario, n, fill = Size), outlier.shape = NA) +
  geom_smooth(aes(Scenario, n, col = Size, group = Size), alpha = 0.2) +
  scale_fill_manual(values = Pal, labels = Labs) +
  scale_color_manual(values = Pal) +
  scale_y_sqrt(breaks = c(1, 10, 23, 40, 61), limits = c(0, 70)) +
  facet_rep_wrap(Ecozone ~ ., repeat.tick.labels = TRUE) +
  ylab('Number of species') +
  monkey_theme +
  theme(text = element_text(size = 40))
  
