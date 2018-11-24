VariableSelection <- function(data,categoryID){
  
require(glmnet)
require(dplyr)
  
data -> allData

allData %>% filter(Random == 'Train') -> train
allData %>% filter(Random == 'Test') -> test

train %>% select(matches('^[DPFp].$')) %>% as.matrix() -> trainX
train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY

test %>% select(matches('^[DPFp].$')) %>% as.matrix() -> testX
test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY

grid <- 10^seq(10,-2,length=100)

lassoModel <- cv.glmnet(trainX, trainY, alpha=1, lambda=grid)
tmp <- coef(lassoModel)
varNames <- rownames(tmp[tmp[,1] != 0,, drop=FALSE])
interIndex <- match(c("(Intercept)"),varNames)

return(varNames[which(varNames != '(Intercept)')])

}

for (categoryID in 1:24){
  print(categoryID)
  
  print(VariableSelection(allData,categoryID))
}
