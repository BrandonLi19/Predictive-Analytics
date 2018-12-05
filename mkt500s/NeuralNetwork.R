NeuralNetwork <- function(data, storeID, categoryID) {
  
  require(neuralnet)
  require(dplyr)
  print(paste("Neural Network for store", storeID, "category", categoryID))
  # f <- function(x) {(x-min(x))/(max(x)-min(x))}  # Is this standardization?
  # Ignore storeID since neural network requires huge amount of data
  # allData %>% filter(StoreID == storeID) -> storeData
  data -> storeData
  
  storeData %>% filter(Random == 'Train') -> train
  # train <- as.data.frame(apply(temptrain[,c(3:122)],2,f)) 
  
  storeData %>% filter(Random == 'Test') -> test
  # test <- as.data.frame(apply(temptest[,c(3:122)],2,f))
  vars <- VariableSelection(data,categoryID)
  train %>% select(vars) -> trainX  # No need to cast as matrix
  test %>% select(vars) -> testX
  
  train %>% select(paste0('Y', categoryID)) -> trainY
  # train %>% select(matches('^[DPF].+$')) -> trainX
  # 
  # test %>% select(matches('^[DPF].+$')) -> testX
  
  test %>% select(paste0('Y', categoryID)) -> testY
  
  # Neural Network
  fml <- as.formula(paste(names(trainY), '~', paste(names(trainX), collapse = '+')))  # Build formula from string
  
  nn <- neuralnet(fml, data = train, linear.output = F, hidden = 2)  # Use formula as first parameter, not data sets
  #prediction
  pred <- compute(nn, testX)
  
  return(list(model = 'NeuralNet', RMSE = sqrt(mean(unlist((testY - pred$net.result) ^ 2))), store = storeID, category = categoryID))
}

