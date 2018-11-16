RegressionTree<- function(data, storeID, categoryID){
  require(glmnet)
  require(dplyr)
  require(tree)
  
  data %>% filter(StoreID == storeID) -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].$')) %>% as.matrix() -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
  train_data = data.frame(trainX,trainY)
  
  
  test %>% select(matches('^[DPF].$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY
  test_data = data.frame(testX,testY)
  
  regressionTreeModel=tree(trainY~.,train_data)
  yhat.RT=predict(regressionTreeModel,newdata=test_data)
  test.RT=test_data[-train,"testY"]
  return (list(model = regressionTreeModel, MSE = mean((unlist(yhat.RT - test.RT)^2))))}