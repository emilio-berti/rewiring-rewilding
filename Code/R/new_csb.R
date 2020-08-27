#maxent_sdm return 0 if there are not enough location points or if the k-fold
#validation fails; returns the model evaluation statistics if it completes
#correctly
maxent_sdm <- function(taxon, map_output){
  # import presence records
  records <- read_csv(paste0(taxon, ".csv"), col_types = cols()) %>%
    dplyr::select(x, y)
  
  # buffer size calculation
  presence <- raster(paste0("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2/Data/Ranges/Current/", taxon, ".tif")) + raster(paste0("/home/GIT/Trophic_restoration/Data/PHYLACINE_1.2/Data/Ranges/Present_natural/", taxon, ".tif"))
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
  
  if(nrow(records) < 10){
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
  if(any(isNa)){
    records <- records[-which(isNa), ]
    records_climate <- records_climate[-which(isNa), ]
  }
  
  # make buffer
  buffer_shp <- buffer(presence, width = buffer * 1000, dissolve = T) 
  # env_pred is a stack with enviromental predictor in the buffer
  env_pred <- crop(climate, extent(buffer_shp)) 
  env_pred <- mask(env_pred, buffer_shp)
  # generate 100,000 background sites in the buffer
  r_bg_points <- tryCatch( 
    {
      randomPoints(env_pred, 100000)
    },
    error = function(e){
      sink(paste0(map_output, "Log.txt"), append = T)
      print(paste0("Error for species ", taxon, " : ", e))
      sink()
      return(NA)
    })
  if(all(is.na(r_bg_points))){
    return(0)
  }
  # extract environment at sites
  r_bg_climate <- raster::extract(climate, r_bg_points) 
  r_bg_climate <- as.data.frame(r_bg_climate)
  # remove any sites with NA for at least one variable
  isNa <- is.na(rowSums(r_bg_climate)) 
  if(any(isNa)){
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
  if(nrow(records) < 5 | nrow(r_bg_points) < 5){
    return(0)
  }
  kfold_presence <- data_maxent %>% filter(presence == 1) %>% kfold(5)
  kfold_background <- data_maxent %>% filter(presence == 0) %>% kfold(5)
  data_maxent$kfold <- c(kfold_presence, kfold_background)
  # error handling to skip species for which maxnet hinge-only fails.
  # output of error will be written in map_output/Log.txt
  # model_stats <- lapply(1:5, k_fold, dataset = data_maxent)
  model_stats <- tryCatch(
    lapply(1:5, k_fold, data_maxent),
    error = function(e){
      sink(paste0(map_output, "Log.txt"), append = T)
      print(paste(taxon, "fails for maxnet with lqh classes:", e))
      sink()
      skip_species <- T
      return(NA)
    }
  )
  if(any(is.na(model_stats))){
    return(0)
  }
  model_stats <- do.call(rbind, model_stats)
  write_csv(model_stats, paste0('~/Documents/new_cbi/SDM_stat_', taxon, '.csv'), col_names = T)
  
  # me <- maxnet(
  #   p = data_maxent$presence,
  #   data = data_maxent[3:6],
  #   f = maxnet.formula(p = data_maxent$presence, data = data_maxent[3:6], classes = 'lqh')
  # )
  # # predict suitability and save raster
  # suitability <- predict(
  #   climate, me, 
  #   type = "cloglog",
  #   filename = paste0(map_output, taxon),
  #   format = "GTiff",
  #   overwrite = T
  # )
  # # dismo::mess(climate, presence[ , 3:6])
  # # suitability_CBI <- suitability >= CBI_Th(dataset = model_stats)
  # suitability_MSS <- suitability >= mean(unique(model_stats$MSS))
  # # suitability_OR10 <- suitability >= mean(unique(model_stats$OR10))
  # # plot map to pdf
  # pdf(paste0(map_output, taxon, ".pdf"), width = 10, height = 4)
  # par(mai = c(0, 0, 0.5, 0))
  # # write binary map to tif
  # writeRaster(suitability_MSS, paste0(map_output, taxon, "_MSS", ".tif"), overwrite = T)
  # plot(suitability_MSS, main = substitute(italic(x), list(x = gsub("_", " ", taxon))), axes = F, box = F, legend = F)
  # dev.off()
  # 
  # metrics <- tibble(
  #   Species = as.character(taxon), 
  #   Spearman.CBI = round(mean(unique(model_stats$Spearman)), 2),
  #   Dev.Spearman.CBI = round(sd(model_stats$Spearman), 2),
  #   AUC = round(mean(unique(model_stats$AUC)), 2),
  #   Dev.AUC = round(sd(model_stats$AUC), 2),
  #   CBI.threshold = round(CBI_Th(dataset = model_stats), 2), #no dev
  #   MSS.threshold = round(mean(unique(model_stats$MSS)), 2), #no dev
  #   OR.10 = round(mean(unique(model_stats$OR10)), 2),
  #   OR.min = round(mean(unique(model_stats$OR_min)), 2),
  #   Dev.OR.min = round(sd(model_stats$OR_min), 2)
  # ) 
  # 
  # # writing to file. This is not necessary as the metrics are returned, however
  # # it assures that in a large loop with many species the statistics are written
  # # to a file in case the algorithm fails for one species.
  # write_csv(metrics, paste0(map_output, "SDM_stat_", taxon, ".csv"), col_names = T)
  
  return(0)
  
}
