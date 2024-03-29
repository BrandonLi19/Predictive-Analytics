LassoRegression <- function(data, storeID, categoryID) {
  require(glmnet)
  require(dplyr)
  print(paste("Lasso for store", storeID, "category", categoryID))
  data %>% filter(StoreID == storeID) -> storeData
  # data -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].$')) %>% as.matrix() -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
  
  test %>% select(matches('^[DPF].$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY
  
  grid <- 10^seq(10,-2,length=100)
  
  lassoModel <- cv.glmnet(trainX, trainY, alpha=0, lambda=grid)
  bestLambdaLasso <- lassoModel$lambda.min
  predictionLasso <- predict(lassoModel, s=bestLambdaLasso, newx=testX)
  
  return(list(model = 'lasso', RMSE = sqrt(mean((predictionLasso - testY)^2)), store = storeID, category = categoryID))
}

