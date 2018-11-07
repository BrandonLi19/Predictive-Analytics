library(tree)
setwd("C:/Users/Xin Guan/Documents/500S/Final_Project")
store_2=read.csv("Store2-1.csv", header = T)
store_2 = na.omit(store_2)
p1_s2x=store_2[,3:6]
p1_s2y=store_2[,99]
p1_s2=data.frame(p1_s2y,p1_s2x)
library(randomForest)
set.seed(1)
bag.p1_s2=randomForest(p1_s2y~.,data=p1_s2,subset=train,mtry=4,importance=TRUE,na.action=na.exclude)
yp1s2hat.bag=predict(bag.p1_s2,newdata=p1_s2[-train,])
p1_s2.test=p1_s2[-train,"p1_s2y"]
plot(yp1s2hat.bag,p1_s2.test)
abline(0,1)
mean((yp1s2hat.bag-p1_s2.test)^2)