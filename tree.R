
library("readxl")

data <- read_excel("/Users/xingxueyan/Desktop/predictive/dataset/Store3.xlsx")


train <- data[data[, "Random"] == "Train",]
test <- data[data[, "Random"] == "Test",]

train_x = as.matrix(train[, c("F1", "D1", "PR1", "P1")])
train_y = as.matrix(train[, c("Y1")])
train_data = data.frame(train_x,train_y)

test_x = as.matrix(test[, c("F1", "D1", "PR1", "P1")])
test_y = as.matrix(test[, c("Y1")])
test_data = data.frame(test_x,test_y)

data1 <- merge(train_data,test_data,all = TRUE)

library(MASS)

set.seed(5)

library(randomForest)

sample_test <- test_data$Y1

train1 = sample(1:nrow(data1),nrow(data1)/2)
test1= data1[-train,]$Y1

# MSE is 183199
rf.sample = randomForest(Y1~.,data=data1,subset=train1,mtry=3,importance = TRUE)
# randomly choose 3 variables

yhat.rf = predict(rf.sample,newdata = test_data)

mean((yhat.rf-sample_test)^2)

varImpPlot(rf.sample)

importance(rf.sample)




# boosted tree

library(MASS)

set.seed(1)

library(gbm)

set.seed(1)

boost.data = gbm(Y1~.,train_data,distribution = "gaussian", n.trees=5000,interaction.depth=4)

summary(boost.data)

yhat.boost=predict(boost.data,newdata=test_data,n.trees=5000)

# could change the number of trees that used in the model
set.seed(5)

sample_data.test=test_data$Y1

mean((yhat.boost-sample_data.test)^2)
#MSE1 is 224382.3

boost.sample_data = gbm(Y1~.,data=train_data,distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
#change the shrink_age
#shrink_age might be the most important parameter that influence the model

yhat.boost=predict(boost.data,newdata=test_data,n.trees=5000)

mean((yhat.boost-sample_data.test)^2)
# the MSE is 224382.3

yhat.boost=predict(boost.data,newdata=test_data,n.trees=500)
# use only 500 trees

mean((yhat.boost-sample_data.test)^2)
# the MSE is 219431.3
```

