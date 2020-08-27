library(tmap)
library(sf)

source("my_theme.R")

data(World)

World <- World %>% 
  st_transform(st_crs(presence)) %>% 
  filter(continent == "Africa") %>% 
  st_buffer(1) %>% 
  summarise()

ggplot() + 
  geom_sf(data = World, fill = "white") +
  geom_sf(data = st_as_sf(presence), fill = "green4", alpha = 0.5) + 
  geom_point(data = as_tibble(center@coords), aes(x, y), col = "black") +
  geom_point(data = as_tibble(d), aes(x, y), col = "black") +
  geom_segment(aes(x = center@coords[1], xend = d$x,
                   y = center@coords[2], yend = d$y)) +
  ggtitle("Ceratotherium simum") +
  annotate("text", label = "italic(d)",
           x = 3000000, y = -1800000,
           angle = 45, parse = T, size = 8) +
  monkey_map

ggsave("/home/GIT/Trophic_restoration/Manuscript/Figures/varbuffer.png", width = 6, height = 6)
