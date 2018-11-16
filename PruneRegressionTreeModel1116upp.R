PruneRegressionTree<- function(data, storeID, categoryID){
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
  
  rpartTreeModel=rpart(trainY~.,train_data,method="anova")
  cp_prune=rpart_tree$cptable[which.min(rpart_tree$cptable[,"xerror"]),"CP"]
  pruneRegressionTreeModel=prune(rpartTreeModel,cp=cp_prune)
  #plot(prune_tree)#
  yhat.prune=predict(pruneRegressionTreeModel,newdata=test_data)
  prune.test=p1_s2[-train,"p1_s2y"]
  #plot(yp1s2hat,p1_s2.test)
  #abline(0,1)
  
  yhat.prune=predict(pruneRegressionTreeModel,newdata=test_data)
  test.prune=test_data[-train,"testY"]
  return (list(model = pruneRegressionTreeModel, MSE = mean((unlist(yhat.prune - test.prune)^2))))}