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

SourceEntireFolder('mkt500s')

# modelList <- list(LassoRegression, BoostedTree, NeuralNetwork, RandomForest)
modelList <- list(LassoRegression, BoostedTree, RandomForest)
fr <- apply(combinations, MARGIN = 1, ModelEval, modelList = modelList, data = allData)
