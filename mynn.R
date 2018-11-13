require(openxlsx)
require(neuralnet)
require(MASS)
data <- read.xlsx("Store2.xlsx")
str(data)
  
f <- function(x) (x-min(x))/(max(x)-min(x))
train <- data[data[, "Random"] == "Train",]
train <- as.data.frame(apply(train[,c(3:122)],2,f))
test <- data[data[, "Random"] == "Test",]
test <- as.data.frame(apply(test[,c(3:122)],2,f))
  
train_x = as.matrix(train[, c("F1", "D1", "PR1", "P1")])
train_y = as.matrix(train[, c("Y1")])
  
test_x = as.matrix(test[, c("F1", "D1", "PR1", "P1")])
test_y = as.matrix(test[, c("Y1")])
  
# Neural Network
set.seed(122)
nn <- neuralnet(train_y~train_x, data = train, linear.output = F, hidden = 2)
nn$weights
nn$net.result
plot(nn, rep="best")
  
#prediction
prediction(nn)
pred <- compute(nn, test_x)
pred$net.result
  
# RMSE Calculation
#rmse = function(actual, predicted) {
#sqrt(mean((actual - predicted) ^ 2))
#}
#rmse(predicted = pred$net.result, actual = test_y)
RMSE =sqrt(mean((test_y - pred$net.result) ^ 2))
RMSE