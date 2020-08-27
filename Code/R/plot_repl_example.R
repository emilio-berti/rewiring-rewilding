library(tidyverse)
library(raster)
library(sf)
library(tmap)


d <- read_csv("../../Results/Replacements.csv") %>% 
  filter(Replacement == "Panthera_leo")

r_ex <- raster("/NewSpace/PN_resampled/5km/Smilodon_fatalis.tif")
r1 <- raster("/NewSpace/PN_resampled/5km/Panthera_leo.tif")
r2 <- raster("/NewSpace/Maps/Maxent/Panthera_leo_MSS.tif")
r <- r1 + r2

r_ex[r_ex == 0] <- NA
r[r >= 1] <- 1
r[r < 1] <- NA
r <- r * r_ex

realm <- read_sf("../../Data/Ecozone.shp") %>% 
  filter(REALM %in% c("NA")) %>% 
  st_transform(st_crs(r1)) %>% 
  st_simplify(dTolerance = 1000) %>% 
  st_crop(xmin = -17809983, xmax = -594653.2, 
          ymin = 1000481, ymax = 7986971)

r_r <- fasterize::fasterize(realm, r_ex)
r_ex <- r_ex * r_r
r <- r * r_r

pdf("../../Manuscript/Figures/example_range.pdf", width = 9, height = 6)
plot(realm$geometry)
plot(r_ex, add = TRUE, legend.show = FALSE,
     col = c(rgb(0, 0, 0, 0), rgb(0, 0.6, 0, 0.5)))
plot(r, add = TRUE, legend.show = FALSE,
     col = c(rgb(0, 0, 0, 0), rgb(0, 0, 0.6, 0.5)))
plot(realm$geometry, add = TRUE)
dev.off()
