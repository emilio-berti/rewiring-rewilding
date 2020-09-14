#' DONE
#' This function computes the cross-validation statistics for the maxnet SDM.
#' @param K is the fold.
#' @param dataset is the data.frame on which to run the maxnet model.
#' @return a data.frame with the evaluation statistics.
k_fold <- function(K, dataset){
  train <- dataset %>% filter(kfold != K)
  test <- dataset %>% filter(kfold == K)
  me <- maxnet(p = train$presence, data = train[3:6],
               f = maxnet.formula(p = train$presence, 
                                  data = train[3:6], 
                                  classes = 'lqh'))
  # continuous boyce index
  suitability_obs <- predict(me, 
                             train[train$presence == 1, 3:6], 
                             type = "cloglog")
  suitability_test <- predict(me, 
                              test[3:6], 
                              type = "cloglog")
  cbi <- ecospat.boyce(suitability_test, 
                       suitability_test[test$presence == 1], 
                       res = 100, PEplot = F)
  HS <- cbi$HS
  PE <- cbi$F.ratio
  cbi <- cbi$Spearman.cor
  # AUC
  suitability_abs <- predict(me, 
                             test[which(train$presence == 1), 3:6],
                             type = "cloglog")
  eval <- evaluate(as.vector(suitability_obs),
                   as.vector(suitability_abs))
  auc <- eval@auc
  # ORMin - model overfitting
  OR_min <- min(suitability_obs)
  # OR10
  OR_ten <- sort(suitability_obs)[ceiling(length(suitability_obs) / 10)]
  # MSS
  MSS <- eval@t[which.max(eval@TPR + eval@TNR)]
  return(
    data.frame(
      HS = HS, 
      PE = PE, 
      Spearman = rep(cbi, length(PE)), 
      AUC = rep(auc, length(PE)),
      MSS = rep(MSS, length(PE)),
      OR10 = rep(OR_ten, length(PE)),
      OR_min = rep(OR_min, length(PE))
    )
  )
}