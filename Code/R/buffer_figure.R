library(tidyverse)
library(raster)
library(tmap)
library(sf)

taxon <- "Ceratotherium simum"
records <- read_csv("/NewSpace/Presence_location/Ceratotherium_simum.csv") %>%
  dplyr::select(x, y)

presence <- raster("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2/Data/Ranges/Current/Ceratotherium_simum.tif") + raster("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2/Data/Ranges/Present_natural/Ceratotherium_simum.tif")
presence[presence > 1] <- 1
presence[presence == 0] <- NA
presence <- rasterToPolygons(presence, dissolve = T) 
presence <- buffer(presence, width = 1, dissolve = T) 
largest <- which.max(sapply(1:length(presence@polygons[[1]]@Polygons), function(x) presence@polygons[[1]]@Polygons[[x]]@area))
hull <- dismo::convHull(presence@polygons[[1]]@Polygons[[largest]]@coords)
center <- rgeos::gCentroid(hull@polygons)
hull <- hull@presence
distance <- sqrt((center@coords[ , 1] - hull[ , 1])^2 + (center@coords[ , 2] - hull[ , 2])^2)
d <- hull[which(distance == max(distance))[1], ]
buffer <- sqrt(d$x^2 + d$y^2) / 1000

data("World")
africa <- World %>% 
  filter(continent == "Africa") %>% 
  st_simplify(dTolerance = 100) %>% 
  st_transform(st_crs(presence)) %>% 
  st_buffer(dist = 100) %>% 
  summarise()

p <- presence %>% 
  st_as_sf() %>% 
  st_cast("POLYGON") %>% 
  mutate(n = 1:2)

buffer_shp <- buffer(presence, width = buffer * 1000, dissolve = T) %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = africa, alpha = 0.0) +
  geom_sf(data = p[1, ], show.legend = FALSE, alpha = 0.5) +
  geom_sf(data = p[2, ], show.legend = FALSE, alpha = 0.5, fill = "green4") +
  geom_line(aes(x = c(2505254, 3955939), y = c(-2686016, -1241448)), size = 1) +
  geom_point(aes(x = 2505254, y = -2686016), col = "steelblue", size = 2) +
  geom_point(aes(x = 3955939, y = -1241448), col = "tomato", size = 2) +
  ggtitle(taxon) +
  theme_map() +
  theme(plot.title = element_text(face = 4, hjust = 0.5))

ggsave("../../Manuscript/Figures/rhyno_buffer.png", width = 6, height = 6)
