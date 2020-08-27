#import and load packages
library(maxnet)
library(raster)
library(dismo)
library(tidyverse)

setwd("/NewSpace/Behrmann/v1")

# import environmental layers 
climate <- stack(c("bio5.tif", "bio6.tif", "bio16.tif", "bio17.tif"))
names(climate) <- c("MaxTemp", "MinTemp", "PrecWet", "PrecDry")
# plot(climate)

setwd("/NewSpace/Presence_location")

# import presence records
records_csv <- read.csv("Felis_silvestris.csv")
records_csv <- dplyr::select(records_csv, longitude = x, latitude = y)

# extract environment at sites
records_extract <- raster::extract(climate, records_csv[c("longitude", "latitude")])

# remove any sites with NA for at least one variable
isNa <- is.na(rowSums(records_extract))
if(any(isNa)) {
  records_extract <- records_extract[-which(isNa), ]
}

# Make buffer at 500km
# USE THE RESAMPLED PN HERE
pn <- raster("/home/GIT/Megalinkers/Data/PHYLACINE_1.2.0/Data/Ranges/Present_natural/Felis_silvestris.tif") 
cu <- raster("/home/GIT/Megalinkers/Data/PHYLACINE_1.2.0/Data/Ranges/Current/Felis_silvestris.tif") 
presence <- sum(pn, cu, na.rm = T)
presence <- presence / presence
extent(presence) <- extent(climate)
presence <- projectRaster(presence, climate)
presence[presence == 0] <- NA

rasterOptions(tmpdir = "/tmp/R/rasters")
rasterOptions(maxmemory = 10^6)

presence_buffer <- buffer(presence, 500000)

presence_buffer <- rasterToPolygons(presence, dissolve = T)

pn_buffer <- rgeos::gBuffer(pn_buffer, width = 500000)

plot(pn_buffer)

# generate 100,000 background sites
randomBgSites <- dismo::randomPoints(climate * pn, 100000)

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
randomBg <- randomBg %>% dplyr::rename(longitude = x, latitude = y)

# bind rows
trainDataAuto <- rbind(
  records_extract,
  randomBgEnv
)

# We also need a vector of 1's and 0's, one value per row in trainData to indicate if it's a presence or a background site.
presBg <- c(rep(1, nrow(records_extract)), 
            rep(0, nrow(randomBgEnv)))

# model species: note we're using the new "maxnet" function, not the older "maxent" function!
#t0 <- Sys.time()

# k-fold data partitioning 
pres <- records_extract
back <- randomBgEnv

# the background data will only be used for model testing and does not need to be partitioned. We now partition the data into 5 groups.
k <- 5
group <- kfold(pres, k)

# now we can fit and test our model five times. In each run, the records corresponding to one of the five groups is only used to evaluate the model, while the other four groups are only used to fit the model. The results are stored in a list called ‘e’.
e <- list()
for (i in 1:k) {
  train <- pres[group != i, ]
  test <- pres[group == i, ]
  me <- maxnet::maxnet(p = presBg, 
                       data = trainDataAuto, 
                       f = maxnet.formula(p = presBg, 
                                          data = trainDataAuto, 
                                          classes = 'lqh'))
  e[[i]] <- evaluate(p = test, a = back, me)
}   

# will be useful to put other evaluation statistics into the loop above, e.g. continuous boyce index and, omission rate 10 percent, and minimum training presence threshold
auc <- sapply(e, function(x){x@auc}) 
#auc
mean(auc)
sapply(e, function(x){threshold(x)['no_omission']})

# to run for the final model with the full data
autoSelectModel <- maxnet::maxnet(p = presBg, 
                          data = trainDataAuto, 
                          f = maxnet.formula(p = presBg, 
                                             data = trainDataAuto, 
                                             classes = 'lqh'))

# save raster
autoSelectMap <- predict(climate, 
                         autoSelectModel,
                         filename = '~/Desktop/untitled folder/Ailurops_ursinus_cloglog',
                         format = 'GTiff', 
                         overwrite = TRUE, 
                         type = 'cloglog')
#Sys.time() - t0

# plot output
plot(autoSelectMap)
points(records_df$longitude, 
       records_df$latitude, 
       pch = 21, 
       col = "blue")

# evaluation statistics ----
# threshold-dependent metrics of model performance
predPres <- raster::extract(autoSelectMap, 
                            cbind(records_df$longitude, 
                                  records_df$latitude), 
                            df = TRUE)
predPres <- predPres %>% na.omit()
predBg <- raster::extract(autoSelectMap, 
                          cbind(randomBg$longitude, 
                                randomBg$latitude), 
                          df = TRUE)
predBg <- predBg %>% na.omit()

# Minimum training presence threshold
# Description: The smallest predicted value across all of the training presences.
# Notes: This threshold guarantees every training presence is predicted to be "present". However, this threshold is often very optimistic and over-predicts the species' range.

minTrainPresThreshold <- min(predPres)
minTrainPresThreshold

# Threshold that maximizes the sum of sensitivity and specificity
# calculate an "evaluation" object
eval <- evaluate(p = as.vector(predPres$Ailurops_ursinus_cloglog), 
                 a = as.vector(predBg$Ailurops_ursinus_cloglog), 
                 tr = seq(0, 1, by = 0.01))
eval 
# Description: This threshold maximizes the correct prediction rate regardless of whether correct predictions are mainly from presences or absences/background sites
# Notes: Assuming test presences and background sites are sampled from the landscape in a representative manner (i.e., unbiased), the threshold value that maximizes the sum of sensitivity and specificity will be the same regardless of whether true absences or background sites are use. Pretty cool! See Liu, C., White, M., and Newell, G. 2013. Selecting thresholds for the prediction of species occurrence with presence-only data. Journal of Biogeography 40:788-789.
maxSeSpThreshold <- eval@t[which.max(eval@TPR + eval@TNR)]
maxSeSpThreshold

# Now, let's look at what the maps look like when they're thresholded.
par(mfrow=c(1, 2), pty='s')

thresholdedMapMin <- autoSelectMap >= minTrainPresThreshold
thresholdedMapMSS <- autoSelectMap >= maxSeSpThreshold

plot(thresholdedMapMin)
plot(thresholdedMapMSS)

# Area under the receiver-operator characteristic curve (AUC)
eval@auc
threshold(eval)

#The Continuous Boyce Index (CBI)
# from the enmSdm package
cbi <- contBoyce(pres=predPres$Ailurops_ursinus_cloglog, bg=predBg$Ailurops_ursinus_cloglog)
cbi
