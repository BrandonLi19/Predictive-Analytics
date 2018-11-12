ridgeRegression <- function(data, storeID, categoryID) {
  require(glmnet)
  require(dplyr)
  
  data %>% filter(StoreID == storeID) -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].$')) %>% as.matrix() -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
  
  test %>% select(matches('^[DPF].$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY
  
  grid <- 10^seq(10,-2,length=100)
  
  ridgeModel <- cv.glmnet(trainX, trainY, alpha=0, lambda=grid)
  bestLambdaRidge <- ridgeModel$lambda.min
  predictionRidge <- predict(ridgeModel, s=bestLambdaRidge, newx=testX)
  
  return(list(model = ridgeModel, MSE = mean((predictionRidge - testY)^2)))
}
ridgeRegression(allData, 3, 1)
