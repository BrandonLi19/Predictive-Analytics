neuralNet <- function(data, storeID, categoryID) {
  require(openxlsx)
  require(neuralnet)
  require(MASS)
  require(magrittr)
  
  data %>% filter(StoreID == storeID) -> storeData
  f <- function(x) {(x-min(x))/(max(x)-min(x))}
  storeData %>% filter(Random == 'Train') -> temptrain
  train <- as.data.frame(apply(temptrain[,c(3:122)],2,f)) 
  
  storeData %>% filter(Random == 'Test') -> temptest
  test <- as.data.frame(apply(temptest[,c(3:122)],2,f))
  
  train %>% select(matches('^[DPF].+$')) %>% as.matrix() -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
  
  test %>% select(matches('^[DPF].+$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY

  # Neural Network
  nn <- neuralnet(trainY~trainX, data = train, linear.output = F, hidden = 2)
  #prediction
  pred <- compute(nn, testX)
  pred$net.result
  return(list(model = nn, RMSE = sqrt(mean((testY - pred$net.result) ^ 2))))
}
neuralNet(allData, 3, 1)