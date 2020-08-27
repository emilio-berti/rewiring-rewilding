library(rbenchmark)
library(raster)

setwd("/home/GIT/trophic_restoration/Code/R")

rasterOptions(tmpdir = "/home/Rtmp")

# rasters intersections ---------------------------------------------------
simple_raster <- function(){
  res <- disaggr * s
  which(res@data@max == 1)
}

raster_as_matrix <- function(){
  mat <- matrix(disaggr, ncol = 1)
  m <- as.matrix(s)
  res <- apply(m, 2, function(x) max(x * mat))
  res
}

elapsed <- matrix(0, ncol = 3, nrow = 10)
for(i in 1:10){
  print(i)
  r <- raster("../../Data/PHYLACINE_1.2/Data/Ranges/Present_natural/Acinonyx_jubatus.tif")
  disaggr <- disaggregate(r, 20) #this is more or less the same as the final resolution
  species <- sample(list.files("../../Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", full.names = T), i)
  s <- stack(species)
  s <- disaggregate(s, 20)
  T0 <- Sys.time()
  simple_raster()
  T1 <- Sys.time() - T0
  T0 <- Sys.time()
  raster_as_matrix()
  T2 <- Sys.time() - T0
  elapsed[i, ] <- c(i * 10, T1, T2)
}
elapsed[1, 3] <- elapsed[1, 3] / 60

pdf("../../Figures/benchmarking_raster_multiplication.pdf")
plot(elapsed[ , 1], elapsed[ , 2], col = "steelblue", pch = 20, xlab = "Number of species", ylab = "Execution time (minutes)")
points(elapsed[ , 1], elapsed[ , 3], col = "tomato", pch = 20)
legend("right", legend = c("Simple raster", "As matrix"), 
       col = c("steelblue", "tomato"), pch = 20)
dev.off()
