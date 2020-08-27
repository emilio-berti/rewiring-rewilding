library(tidyverse)
library(raster)
library(sf)


realms <- st_read("../../Data/Bioregions.shp")
na <- filter(realms, WWF_REALM == "NA")
phy <- read_csv("../../Data/PHYLACINE_1.2.0/Data/Traits/Trait_data.csv")

ex <- "Smilodon_fatalis"
al <- c("Panthera_leo", "Panthera_onca", "Panthera_tigris")

pn <- raster("../../Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural/Smilodon_fatalis.tif")
reintro <- stack(paste0("../../Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural/", al ,".tif"))
intro <- stack(paste0("/media/squirry/Emilio_HDD/SDM/maxent/variable_buffer/", al,".tif"))
aggr_intro1 <- projectRaster(intro$Panthera_leo, pn, method = "ngb")
aggr_intro2 <- projectRaster(intro$Panthera_tigris, pn, method = "ngb")
intro <- stack(aggr_intro1, aggr_intro2)

# neotropic -----
pn <- crop(pn, na)
reintro <- crop(reintro, na)
intro <- crop(intro, na)

plot(pn, axes = FALSE, box = FALSE, legend = FALSE,
     col = c(rgb(0, 0, 0, 0), "green4"))
plot(na$geometry, add = TRUE)
plot(intro$Panthera_leo * pn, add = TRUE, 
     legend = FALSE, col = c(rgb(0, 0, 0, 0), "gold"))
plot(intro$Panthera_tigris * pn, add = TRUE, 
     legend = FALSE, col = c(rgb(0, 0, 0, 0), rgb(0.8, 0.1, 0.8, 0.5)))

