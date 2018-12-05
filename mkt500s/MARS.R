MARS <- function(data, storeID, categoryID) {
  
  require(earth)
  
  print(paste("MARS for store", storeID, "category", categoryID))
  ## Ignore storeID since MARS requires huge amount of data
  data %>% filter(StoreID == storeID) -> storeData
  # data -> storeData
  storeData %>% filter(Random == 'Train') -> train
  
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].+$')) -> trainX
  train %>% select(paste0('Y', categoryID)) -> trainY
  
  test %>% select(matches('^[DPF].+$')) -> testX
  test %>% select(paste0('Y', categoryID)) -> testY
  
  ## Build model
  ## Choose if we need include interactions?and what degree?
  makeMarsPred <- function(degree = 1, trainX , trainY, testX, testY) {
    MarsModel <- earth(trainX, trainY, degree = degree)
    pred <- predict(MarsModel,testX)
    MSE <- mean(unlist((testY - pred)^2))
  }
  
  degree <- 1:10
  marsTestMse <- sapply(degree, makeMarsPred,
                        trainX = trainX,
                        trainY = trainY,
                        testX = testX,
                        testY = testY)
  bestDegree <- degree[which.min(marsTestMse)]
  bestMarsModel <- earth(trainX, trainY, degree = bestDegree)
  pred <- predict(bestMarsModel,testX)
  
  return(list(model = 'MARS', RMSE = sqrt(mean(unlist((testY - pred) ^ 2))), store = storeID, category = categoryID))
}