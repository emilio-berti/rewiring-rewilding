#!/usr/bin/Rscript

args <- commandArgs(trailingOnly = T)

#import and load packages
library(rgeos, quietly = T, warn.conflicts = F)
library(maxnet, quietly = T, warn.conflicts = F)
library(dismo, quietly = T, warn.conflicts = F)
library(tidyverse, quietly = T, warn.conflicts = F)

setwd("/data_vol/emilio/NewSpace/Behrmann/v1")

# import environmental layers
climate <- stack(c("bio5.tif", "bio6.tif", "bio16.tif", "bio17.tif")) 
names(climate) <- c("MaxTemp", "MinTemp", "PrecWet", "PrecDry")
# plot(climate)

setwd("/data_vol/emilio/NewSpace/Presence_location")

# import presence records
records <- read.csv(paste0(args[1], ".csv"))
records <- records[c("x", "y")]

# extract environment at sites
records_extract <- raster::extract(climate, records[c("x", "y")])

# remove any sites with NA for at least one variable
isNa <- is.na(rowSums(records_extract)) 
if (any(isNa)) {
  records_extract <- records_extract[-which(isNa), ]
}

# Make buffer at 500km USE THE RESAMPLED PN HERE
pn <- raster(paste0("/data_vol/emilio/NewSpace/PHYLACINE_1.2/Data/Ranges/Present_natural/", args[1], ".tif")) 
pn[pn == 0] <- NA 
pn_buffer <- buffer(pn, width = 500000, dissolve = T) 
buffer_shp <- rasterToPolygons(pn_buffer, dissolve = T) 
env_pred <- crop(climate, extent(buffer_shp)) 
env_pred <- mask(env_pred, buffer_shp)

# plot(env_pred)

# generate 100,000 background sites
randomBgSites <- randomPoints(env_pred, 100000)

# extract environment at sites
randomBgEnv <- raster::extract(climate, randomBgSites) 
randomBgEnv <- as.data.frame(randomBgEnv)

# remove any sites with NA for at least one variable
isNa <- is.na(rowSums(randomBgEnv)) 
if (any(isNa)) {
  randomBgSites <- randomBgSites[-which(isNa), ]
  randomBgEnv <- randomBgEnv[-which(isNa), ]
}

# combine with coordinates and rename coordinate fields
randomBg <- cbind(randomBgSites, randomBgEnv) 

# bind rows
trainDataAuto <- rbind(
  records_extract,
  randomBgEnv )

# We also need a vector of 1's and 0's, one value per row in trainData to indicate if it's a presence or a background site.
presBg <- c(rep(1, nrow(records_extract)),
            rep(0, nrow(randomBgEnv)))

# model species: note we're using the new "maxnet" function, not the older "maxent" function! k-fold data partitioning
pres <- records_extract 
back <- randomBgEnv

# the background data will only be used for model testing and does not need to be partitioned. We now partition the data into 5 groups.
k <- 5
group <- kfold(pres, k)

# now we can fit and test our model five times. In each run, the records corresponding to one of the five groups is only used to evaluate the model, while the other four groups are only 
# used to fit the model. The results are stored in a list called ‘e’.
e <- list()
for (i in 1:k) {
  train <- pres[group != i, ]
  test <- pres[group == i, ]
  me <- maxnet(p = presBg,
               data = trainDataAuto,
               f = maxnet.formula(p = presBg,
                                  data = trainDataAuto,
                                  classes = 'lqh'))
  e[[i]] <- evaluate(p = test, a = back, me)
}

# will be useful to put other evaluation statistics into the loop above, e.g. continuous boyce index and, omission rate 10 percent, and minimum training presence threshold
auc <- sapply(e, function(x){x@auc})

# sapply(e, function(x){threshold(x)["no_omission"]})

# to run for the final model with the full data
autoSelectModel <- maxnet(p = presBg,
                          data = trainDataAuto,
                          f = maxnet.formula(p = presBg,
                                             data = trainDataAuto,
                                             classes = 'lqh'))

# save raster
autoSelectMap <- predict(climate, autoSelectModel, type = "cloglog")

# plot output
pdf(paste0("/data_vol/emilio/trophic_restoration/Figures/points_location_", args[1], ".pdf")) 
plot(autoSelectMap) 
points(records$x, records$y,
       pch = 21, cex = 0.1,
       col = rgb(0, 0, 1, 0.5)) 
dev.off()

# evaluation statistics ---- threshold-dependent metrics of model performance
predPres <- raster::extract(autoSelectMap,
                            cbind(records$x,
                                  records$y),
                            df = TRUE) 
predPres <- na.omit(predPres)

predBg <- raster::extract(autoSelectMap,
                          cbind(randomBg$x,
                                randomBg$y),
                          df = TRUE) 
predBg <- na.omit(predBg)

# Minimum training presence threshold Description: The smallest predicted value across all of the training presences. Notes: This threshold guarantees every training presence is predicted 
# to be "present". However, this threshold is often very optimistic and over-predicts the species' range.

minTrainPresThreshold <- min(predPres) 

# Threshold that maximizes the sum of sensitivity and specificity calculate an "evaluation" object
eval <- evaluate(p = as.vector(predPres$layer),
                 a = as.vector(predBg$layer),
                 tr = seq(0, 1, by = 0.01))

# Description: This threshold maximizes the correct prediction rate regardless of whether correct predictions are mainly from presences or absences/background sites Notes: Assuming test 
# presences and background sites are sampled from the landscape in a representative manner (i.e., unbiased), the threshold value that maximizes the sum of sensitivity and specificity will 
# be the same regardless of whether true absences or background sites are use. Pretty cool! See Liu, C., White, M., and Newell, G. 2013. Selecting thresholds for the prediction of species 
# occurrence with presence-only data. Journal of Biogeography 40:788-789.
maxSeSpThreshold <- eval@t[which.max(eval@TPR + eval@TNR)]

# OR10 theshold
vals_10_num <- ceiling(0.1 * nrow(predPres))
vals_10 <- sort(predPres$layer, decreasing = F)[vals_10_num + 1]

# Now, let's look at what the maps look like when they're thresholded.

thresholdedMapMin <- autoSelectMap >= minTrainPresThreshold
thresholdedMapMSS <- autoSelectMap >= maxSeSpThreshold
thresholdedMapOR10 <- autoSelectMap >= vals_10

pdf(paste0("/data_vol/emilio/trophic_restoration/Figures/Maps_", args[1], ".pdf"))
par(mfrow=c(3, 1), pty='s')
plot(thresholdedMapMin, main = "Min")
plot(thresholdedMapMSS, main = "MSS")
plot(thresholdedMapOR10, main = "OR10")
dev.off()

writeRaster(thresholdedMapMSS, paste0("/data_vol/emilio/trophic_restoration/Figures/", args[1], "_MSS.tiff"), overwrite = T)
writeRaster(thresholdedMapOR10, paste0("/data_vol/emilio/trophic_restoration/Figures/", args[1], "_OR10.tiff"), overwrite = T)

#The Continuous Boyce Index (CBI) from the ecospat package
cbi <- ecospat::ecospat.boyce(na.omit(autoSelectMap@data@values), predPres$layer)$Spearman.cor

eval_stat <- cbind(args[1], mean(auc), sd(auc), minTrainPresThreshold, maxSeSpThreshold, vals_10, cbi)
write.table(eval_stat, "/data_vol/emilio/trophic_restoration/Results/evaluation_statistics.csv", col.names = F, row.names = F, append = T)
