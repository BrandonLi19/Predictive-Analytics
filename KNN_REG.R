library(openxlsx)
library(Metrics)
library(FNN)
data = read.xlsx("Store2.xlsx")
str(data)

train <- data[data[, "Random"] == "Train",]
test <- data[data[, "Random"] == "Test",]

train_x = as.matrix(train[, c("F1", "D1", "PR1", "P1")])
train_y = as.matrix(train[, c("Y1")])

test_x = as.matrix(test[, c("F1", "D1", "PR1", "P1")])
test_y = as.matrix(test[, c("Y1")])


set.seed(10)
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

make_knn_pred = function(k = 1, training, predicting) {
  pred = FNN::knn.reg(train = train_x, 
                      test = test_x, 
                      y = train_y, k = k)$pred
  act = test_y
  rmse(predicted = pred, actual = act)
}

k = 1:100
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, 
                      training = train_y, 
                      predicting = test_y)
best_k = k[which.min(knn_tst_rmse)]
best_k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))
# summarize results
knn_results = data.frame(
  k,
  round(knn_tst_rmse, 2),
  fit_status
)
colnames(knn_results) = c("k", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)