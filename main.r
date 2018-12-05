library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

source('helper.R')
SourceEntireFolder('mkt500s')


ModelEval <- function(cl, pair, modelList, data){
  evalResult <- parLapply(cl, modelList, function(f, data, storeID, categoryID) {tryCatch({do.call(f, list(data = data,
                                                              storeID = storeID,
                                                              categoryID = categoryID))},error=function(e){print(e)}
  )}, data = data, storeID = pair['store'], categoryID = pair['category'])
  df <- do.call(rbind, evalResult)
  return(df)
}


allData <- DataPrep()

# Generate all combinations of categories and stores
categoryList <- seq.int(from = 1, to = 24)
storeList <- seq.int(from = 2, to = 2)
combinations <- expand.grid(store = storeList, category = categoryList)

modelList <- list(MARS, LassoRegression, BoostedTree, NeuralNetwork, RandomForest)
# modelList <- list(LassoRegression, BoostedTree, RandomForest)


fr <- apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData, cl=cl)

stopCluster(cl)

tmp <- do.call(rbind, fr)

write.csv(tmp,'result.csv')
