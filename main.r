## finds all .R files within a folder and soruces them
SourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}

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






# Generate all combinations of categories and stores
categoryList <- seq.int(from = 1, to = 24)
storeList <- seq.int(from = 2, to = 9)
combinations <- expand.grid(store = storeList, category = categoryList)

SourceEntireFolder('mkt500s')

# modelList <- list(LassoRegression, BoostedTree, NeuralNetwork, RandomForest)
modelList <- list(LassoRegression, BoostedTree, RandomForest)
fr <- apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData)
