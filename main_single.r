source('helper.R')
SourceEntireFolder('mkt500s')


ModelEval <- function(pair, modelList, data){
  evalResult <- lapply(modelList, function(f, data, storeID, categoryID) {tryCatch({do.call(f, list(data = data,
                                                              storeID = storeID,
                                                              categoryID = categoryID))},error=function(e){print(e)}
  )}, data = data, storeID = pair['store'], categoryID = pair['category'])
  df <- do.call(rbind, evalResult)
  return(df)
}


allData <- DataPrep()

# Generate all combinations of categories and stores
categoryList <- seq.int(from = 1, to = 24)
storeList <- seq.int(from = 2, to = 9)
combinations <- expand.grid(store = storeList, category = categoryList)

modelList <- list(MARS, LassoRegression, BoostedTree, NeuralNetwork, RandomForest)
# modelList <- list(LassoRegression, BoostedTree, RandomForest)


fr <- apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData)

# stopCluster(cl)

tmp <- do.call(rbind, fr)

write.csv(tmp,'result.csv')
