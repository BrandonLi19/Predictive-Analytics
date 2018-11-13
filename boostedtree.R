# boosted tree
library(MASS)
library(gbm)
set.seed(1)


BoostedTree <- function(data, storeID, categoryID) {
  require(glmnet)
  require(dplyr)

data %>% filter(StoreID == storeID) -> storeData
storeData %>% filter(Random == 'Train') -> train
storeData %>% filter(Random == 'Test') -> test

train %>% select(matches('^[DPF].$')) %>% as.matrix() -> trainX
train %>% select(paste0('Y', categoryID)) %>% as.matrix() -> trainY
train_data = data.frame(trainX,trainY)


test %>% select(matches('^[DPF].$')) %>% as.matrix() -> testX
test %>% select(paste0('Y', categoryID)) %>% as.matrix() -> testY
test_data = data.frame(testX,testY)

Y = paste0('Y', categoryID)

boost.data0 = gbm(Y~.,train_data,distribution = "gaussian", n.trees=5000,interaction.depth=4)

yhat.boost0=predict(boost.data0,newdata=test_data,n.trees=3000)

# could change the number of trees that used in the model

sample_data.test=test_data$Y

a0 = mean((yhat.boost0-sample_data.test)^2)
#MSE1 is 223941.4

boost.data = gbm(Y~.,data=train_data,distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
#change the shrink_age
#shrink_age might be the most important parameter that influence the model
sample_data.test=test_data$Y

yhat.boost=predict(boost.data,newdata=test_data,n.trees=5000)

a1 = mean((yhat.boost-sample_data.test)^2)
# the MSE is 224382.3

yhat.boost=predict(boost.data,newdata=test_data,n.trees=500)
# use only 500 trees

a2 = mean((yhat.boost-sample_data.test)^2)
# the MSE is 219431.3

return(list(model = boost.sample_data, MSE = min(a0,a1,a2)))
}
BoostedTree(allData, 3, 1)