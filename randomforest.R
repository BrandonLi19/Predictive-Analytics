
RandomForest<- function(data, storeID, categoryID) {
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

data1 <- merge(train_data,test_data,all = TRUE)

library(MASS)

library(randomForest)

set.seed(2)

train1 = sample(1:nrow(data1),nrow(data1)/2)

data.test=data1[-train1,Y]

set.seed(2)
# MSE is 131756.5
rf.sample = randomForest(Y~.,data=data1,subset=train1,mtry=3,importance = TRUE)
# randomly choose 3 variables

yhat.rf = predict(rf.sample,newdata = data1[-train1,])

mean((yhat.rf-data.test)^2)

varImpPlot(rf.sample)

importance(rf.sample)

return(list(model = rf.sample, MSE = mean((yhat.rf-data.test)^2)))
}
RandomForest(allData, 3, 1)
```

