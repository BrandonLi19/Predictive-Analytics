library(tree)
library(randomForest)

BaggingTree.<- function(data, storeID, categoryID){
  require(glmnet)
  require(dplyr)
  
  data %>% filter(StoreID == storeID) -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].$')) %>% as.matrix() -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
  train_data = data.frame(trainX,trainY)
  
  
  test %>% select(matches('^[DPF].$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY
  test_data = data.frame(testX,testY)
  
  baggingModel=randomForest(trainY~.,data=train_data,mtry=96,importance=TRUE,na.action=na.exclude)
  yphat.bag=predict(baggingModel,newdata=test_data)
  data.test=test_data[-train,"p1_s2y"]
  return (list(model = baggingModel, MSE = mean((unlist(yhat.bag - testY)^2))))}