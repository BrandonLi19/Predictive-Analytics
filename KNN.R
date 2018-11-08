library(openxlsx)
store2 = read.xlsx("Store2.xlsx")
store2 = store2[,-1]
store2 = store2[,-1]
str(store2)
fix(store2)
store2_train= store2[1:249,1:4]
store2_test = store2[250:311,1:4]
library(class)
set.seed(10)
k_to_try = 1:100
library(Metrics)
for (i in seq_along(k_to_try)) {
  store2_pred = knn(store2_train, store2_test, store2[1:249,97],k=k_to_try[i])
  rmse(store2[1:249,97], store2_pred)
}