BoostedTree <- function(data, storeID, categoryID) {
  
  require(gbm)
  require(dplyr)
  print(paste("Boosted Tree for store", storeID, "category", categoryID))
  # data %>% filter(StoreID == storeID) -> storeData
  data -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  vars <- VariableSelection(data,categoryID)
  train %>% select(vars) -> trainX
  test %>% select(vars) -> testX
  
  # train %>% select(matches('^[DPF].$')) -> trainX
  # test %>% select(matches('^[DPF].$')) -> testX
  
  train %>% select(paste0('Y', categoryID)) -> trainY
  trainData <- data.frame(trainX,trainY)
  
  
  
  test %>% select(paste0('Y', categoryID)) -> testY
  
  Y = paste0('Y', categoryID)
  
  boost <- gbm(as.formula(paste0(Y, '~.')),data=trainData,distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
  
  #shrink_age might be the most important parameter that influence the model
  
  yhat <- predict(boost,newdata=testX,n.trees=5000)
  
  return(list(model = 'boosted', RMSE = sqrt(mean((unlist(yhat - testY)^2))), store = storeID, category = categoryID))
}