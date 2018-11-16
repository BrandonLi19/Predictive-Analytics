NeuralNetwork <- function(data, storeID, categoryID) {
  
  require(neuralnet)
  require(dplyr)
  
  f <- function(x) {(x-min(x))/(max(x)-min(x))}  # Is this standardization?
  # Ignore storeID since neural network requires huge amount of data
  # allData %>% filter(StoreID == storeID) -> storeData
  data -> storeData
  storeData %>% filter(Random == 'Train') -> temptrain
  train <- as.data.frame(apply(temptrain[,c(3:122)],2,f)) 
  
  storeData %>% filter(Random == 'Test') -> temptest
  test <- as.data.frame(apply(temptest[,c(3:122)],2,f))

  train %>% select(matches('^[DPF].+$')) -> trainX  # No need to cast as matrix
  train %>% select(paste0('Y', categoryID)) -> trainY

  test %>% select(matches('^[DPF].+$')) -> testX
  test %>% select(paste0('Y', categoryID)) -> testY

  # Neural Network
  fml <- as.formula(paste(names(trainY), '~', paste(names(trainX), collapse = '+')))  # Build formula from string
  
  nn <- neuralnet(fml, data = train, linear.output = F, hidden = 2)  # Use formula as first parameter, not data sets
  #prediction
  pred <- compute(nn, testX)
  
  return(list(model = nn, MSE = mean(unlist((testY - pred$net.result) ^ 2))))  # Need to unlist prediction result
}

