source('helper.R')
SourceEntireFolder('mkt500s')

ModelEval <- function(pair, modelList, data){
  evalResult <- lapply(modelList, function(f) {do.call(f, list(data = data,
                                                              storeID = pair['store'],
                                                              categoryID = pair['category']))})
  bestModelIndex <- which.min(unlist(lapply(evalResult, function(entry) {entry$MSE})))
  bestModel <- evalResult[[bestModelIndex]]
  bestModel$storeID <- pair['store']
  bestModel$categoryID <- pair['category']
  return(bestModel)
}


allData <- DataPrep()

# Generate all combinations of categories and stores
categoryList <- seq.int(from = 1, to = 24)
storeList <- seq.int(from = 2, to = 9)
combinations <- expand.grid(store = storeList, category = categoryList)

# modelList <- list(LassoRegression, BoostedTree, NeuralNetwork, RandomForest)
modelList <- list(LassoRegression, BoostedTree, RandomForest)


fr <- apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData)
