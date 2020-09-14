#' DONE
#' Maxent SDM algorithm.
#' @param taxon is the species to be modelled.
#' @param map_output is where to store the predictions.
#' @return 0 if there are not enough location occurrence records or if the k-fold validation fails. 
#' @return the model evaluation statistics if it completes successfully.
maxent_sdm <- function(taxon, map_output) {
  records <- read_csv(paste0(taxon, ".csv"), col_types = cols()) %>%
    dplyr::select(x, y)
  # buffer size calculation ------
  presence <- raster(paste0("/home/GIT/rewiring-rewilding/Data/PHYLACINE_1.2/Data/Ranges/Current/", taxon, ".tif")) + 
    raster(paste0("/home/GIT/rewiring-rewilding/Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", taxon, ".tif"))
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
  
  if (nrow(records) < 10) {
    message(paste(taxon, "has less than 10 points"))
    sink(paste0(map_output, "Log.txt"), append = T)
    print(paste(taxon, "has less than 10 points"))
    sink()
    return(0)
  }
  # extract environment at sites
  records_climate <- raster::extract(climate, records)
  # remove any sites with NA for at least one variable
  isNa <- is.na(rowSums(records_climate)) 
  if (any(isNa)) {
    records <- records[-which(isNa), ]
    records_climate <- records_climate[-which(isNa), ]
  }
  buffer_shp <- buffer(presence, 
                       width = buffer * 1000, 
                       dissolve = T) 
  env_pred <- crop(climate, extent(buffer_shp)) 
  env_pred <- mask(env_pred, buffer_shp)
  # generate 100,000 background sites in the buffer
  r_bg_points <- tryCatch( 
    {
      dismo::randomPoints(env_pred, 100000)
    },
    error = function(e) {
      sink(paste0(map_output, "Log.txt"), append = T)
      print(paste0("Error for species ", taxon, " : ", e))
      sink()
      return(NA)
    })
  if (all(is.na(r_bg_points))) {
    return(0) #return error code 0
  }
  r_bg_climate <- raster::extract(climate, r_bg_points) 
  r_bg_climate <- as.data.frame(r_bg_climate)
  # remove any sites with NA for at least one variable
  isNa <- is.na(rowSums(r_bg_climate)) 
  if (any(isNa)) {
    r_bg_climate <- r_bg_climate[-which(isNa), ]
    r_bg_points <- r_bg_points[-which(isNa), ]
  }
  # combine with coordinates
  presence <- cbind(records, records_climate)
  r_bg <- cbind(r_bg_points, r_bg_climate) 
  # concatenate all data into one dataframe
  data_maxent <- rbind(presence, r_bg)
  # presence (1) or backgroung (0) column
  data_maxent$presence <- c(
    rep(1, nrow(presence)),
    rep(0, nrow(r_bg))
  )
  if (nrow(records) < 5 | nrow(r_bg_points) < 5) {
    return(0) #return error code 0
  }
  kfold_presence <- data_maxent %>% 
    filter(presence == 1) %>% 
    kfold(5)
  kfold_background <- data_maxent %>% 
    filter(presence == 0) %>% 
    kfold(5)
  data_maxent$kfold <- c(kfold_presence, kfold_background)
  # Cross validation ------------------------------------------------- Some
  # species fail with maxnet having hinge as the only feature. Errors output is
  # written in map_output/Log.txt
  model_stats <- tryCatch(
    lapply(1:5, k_fold, data_maxent),
    error = function(e){
      sink(paste0(map_output, "Log.txt"), append = T)
      print(paste(taxon, "fails for maxnet with qlh classes:", e))
      sink()
      skip_species <- T
      return(NA)
    }
  )
  if (any(is.na(model_stats))){
    return(0)
  }
  model_stats <- do.call(rbind, model_stats)
  me <- maxnet(
    p = data_maxent$presence,
    data = data_maxent[3:6],
    f = maxnet.formula(p = data_maxent$presence, 
                       data = data_maxent[3:6], 
                       classes = 'lqh')
  )
  # predict suitability and save raster
  suitability <- predict(
    climate, me, 
    type = "cloglog",
    filename = paste0(map_output, taxon),
    format = "GTiff",
    overwrite = TRUE
  )
  suitability_MSS <- suitability >= mean(unique(model_stats$MSS))
  writeRaster(suitability_MSS, paste0(map_output, taxon, "_MSS", ".tif"), overwrite = T)
  # also in pdf for easier inspection
  pdf(paste0(map_output, taxon, ".pdf"), width = 10, height = 4)
  par(mai = c(0, 0, 0.5, 0))
  # write binary map to tif
  plot(suitability_MSS, 
       main = substitute(italic(x), list(x = gsub("_", " ", taxon))), 
       axes = FALSE, 
       box = FALSE, 
       legend = FALSE)
  dev.off()
  
  metrics <- tibble(
    Species = as.character(taxon), 
    Spearman.CBI = round(mean(unique(model_stats$Spearman)), 2),
    Dev.Spearman.CBI = round(sd(model_stats$Spearman), 2),
    AUC = round(mean(unique(model_stats$AUC)), 2),
    Dev.AUC = round(sd(model_stats$AUC), 2),
    CBI.threshold = round(CBI_Th(dataset = model_stats), 2), #no dev
    MSS.threshold = round(mean(unique(model_stats$MSS)), 2), #no dev
    OR.10 = round(mean(unique(model_stats$OR10)), 2),
    OR.min = round(mean(unique(model_stats$OR_min)), 2),
    Dev.OR.min = round(sd(model_stats$OR_min), 2)
  ) 
  # Writing to file. This is not necessary as the metrics are returned, however
  # it assures that in a large loop with many species the statistics are written
  # to a file in case the algorithm fails for one species.
  write_csv(metrics, paste0(map_output, "SDM_stat_", taxon, ".csv"), col_names = T)
  return(metrics)
}
