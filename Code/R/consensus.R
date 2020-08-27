plot_consensus <- function(x, taxon){
  plot(x, axes = F, box = F, legend = F, 
       col = c("gray95", "orange", "plum2", "green3"),
       main = gsub("_", " ", taxon))
  points(-17400000 / 2, -7000000, pch = 15, cex = 2, col = "orange")
  text(-14400000 / 2, -7000000, "500 km")
  points(0, -7000000, pch = 15, cex = 2, col = "plum2")
  text(3400000 / 2, -7000000, "4000 km")
  points(17400000 / 2, -7000000, pch = 15, cex = 2, col = "green3")
  text(21400000 / 2, -7000000, "Consensus")
}

consensus <- function(x, figure = F){
  x500 <- raster(paste0("/NewSpace/Maps/500km/", x, "_MSS.tif"))
  x4000 <- raster(paste0("/NewSpace/Maps/4000km/", x, "_MSS.tif"))
  n500 <- sum(values(x500), na.rm = T)
  n4000 <- sum(values(x4000), na.rm = T)
  y <- x500 * x4000
  ny <- sum(values(y), na.rm = T)
  if(figure){
    s <- x500 + 2 * x4000
    plot_consensus(x = s, taxon = x)
  }
  write_csv(tibble(x, ny / n500, ny / n4000), "/NewSpace/Maps/Consensus.csv", append = T, col_names = F)
  return(c(ny / n500, ny / n4000))
}


# set temporary directories
Sys.setenv(R_SESSION_TMPDIR = "/NewSpace/Temp_R/")
rasterOptions(tmpdir = "/NewSpace/Temp_R/")
rasterOptions(maxmemory = 150 * 10^6)

species500 <- list.files("/NewSpace/Maps/500km/", pattern = "pdf") %>% gsub(".pdf", "", .)
species4000 <- list.files("/NewSpace/Maps/4000km/", pattern = "pdf") %>% gsub(".pdf", "", .)
species <- intersect(species500, species4000)

done <- read_csv("/NewSpace/Maps/Consensus.csv", col_names = F)$X1
species <- setdiff(species, done)

registerDoParallel(6)
foreach(x = species, .combine = rbind) %dopar%{
  consensus(x, figure = F)
}
stopImplicitCluster()


# Figure ------------------------------------------------------------------
consensus <- read_csv("/NewSpace/Maps/Consensus.csv", col_types = cols(), col_name = c("Species", "500km", "4000km")) %>% 
  mutate(
    Mass = log10(as.numeric(modify(Species, function(x) get_mass(x)$Mass)))
  )

consensus %>% 
  # summarise(
  #   Mean500 = mean(`500km`),
  #   Sd500 = sd(`500km`),
  #   Mean4000 = mean(`4000km`),
  #   Sd4000 = sd(`4000km`)
  # )
  ggplot() +
  geom_point(aes(`500km`, `4000km`), alpha = 0.2) 
