RandomForest <- function(data, storeID, categoryID) {
  
  require(dplyr)
  require(randomForest)
  
  data %>% filter(StoreID == storeID) -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].$')) -> trainX
  train %>% select(paste0('Y', categoryID)) -> trainY
  
  trainData <- data.frame(trainX,trainY)
  
  test %>% select(matches('^[DPF].$')) -> testX
  test %>% select(paste0('Y', categoryID)) -> testY
  
  testData <- data.frame(testX,testY)
  
  Y <- paste0('Y', categoryID)
  # 
  # data1 <- merge(trainData,testData,all = TRUE)
  # train1 = sample(1:nrow(data1),nrow(data1)/2)
  # 
  # dataTest=data1[-train1,Y]
  
  rf <- randomForest(as.formula(paste0(Y, '~.')),data=trainData,mtry=3,importance = TRUE)  # randomly choose 3 variables
  yhat <- predict(rf,newdata = testData)
  
  return(list(model = rf, MSE = mean((unlist(yhat - testY)^2))))
}

RandomForest(allData, 3, 1)

