

ModelEval <- function(pair, modelList, data){
  evalResult <- lapply(modelList, function(f) {do.call(f, list(data = data,
                                                              storeID = pair['store'],
                                                              categoryID = pair['category']))})
  bestModelIndex <- which.min(unlist(lapply(evalResult, function(entry) {entry[[1]][['MSE']]})))
  bestModel <- evalResult[[bestModelIndex]]
  bestModel$storeID <- pair['store']
  bestModel$categoryID <- pair['category']
  return(bestModel)
}


RidgeRegression <- function(data, storeID, categoryID) {
  
  require(glmnet)
  require(dplyr)
  
  data %>% filter(StoreID == storeID) -> storeData
  storeData %>% filter(Random == 'Train') -> train
  storeData %>% filter(Random == 'Test') -> test
  
  train %>% select(matches('^[DPF].+$')) %>% as.matrix()  -> trainX
  train %>% select(paste0('Y', categoryID)) %>% as.matrix()  -> trainY
  
  
  test %>% select(matches('^[DPF].+$')) %>% as.matrix() -> testX
  test %>% select(paste0('Y', categoryID)) %>% as.matrix()  -> testY
  
  # Begin ridge regression
  grid <- 10^seq(10,-2,length=100)
  
  ridgeModel <- cv.glmnet(trainX, trainY, alpha=0, lambda=grid)
  bestLambda <- ridgeModel$lambda.min
  prediction <- predict(ridgeModel, s=bestLambda, newx=testX)
  
  return(list(model = ridgeModel, MSE = mean((prediction - testY) ^ 2)))
  
}


# Read all xlsx workbooks
library(readxl)

fileList <- list.files(pattern = '*.xlsx')
dataList <- sapply(fileList, read_xlsx, simplify = FALSE)
names(dataList) <- tolower(substr(names(dataList), 1, 6))  # Rename list items to 'store2'-like

# GetStoreIDMapping <- function(dataList) {
#   # May not be necessary
#   lapply(dataList, function(dataFrame) {unique(dataFrame$Store)})
# }

# Concatenate store data and add an additional column StoreID ranging from 2 to 9
addIDColumn <- function(storeID) {
  return(cbind(dataList[[storeID]], StoreID = substr(storeID, 6, 6)))
}

dataFrameWithStoreID <- lapply(names(dataList), addIDColumn)
allData <- do.call(rbind, dataFrameWithStoreID)

# Generate all combinations of categories and stores
categoryList <- seq.int(from = 1, to = 24)
storeList <- seq.int(from = 2, to = 9)
combinations <- expand.grid(store = storeList, category = categoryList)

modelList <- list(RidgeRegression)
apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData)
