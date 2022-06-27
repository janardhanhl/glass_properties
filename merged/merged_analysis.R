sciglass <- read.csv('../sciglass/RI_sciglass.csv')
View(sciglass)
summary(sciglass)
head(sciglass)
interglass <- read.csv('../interglad/RI_interglad.csv')
library(sqldf) # sql plugin for writing sql like queiry
master<-sqldf("select * from sciglass 
      union 
      select * from interglass")
master <- sqldf("select * from master 
                where RI > 1.4 and RI <= 2.4 ")
#library(tidyverse)
#detach("package:tidyverse", unload=TRUE)
master[duplicated(master),]

summary(master)
dim(master)
View(master)

# Gradient boosting ----
library(gbm)
set.seed(1)
train<-sample (1: nrow (master), nrow (master)/2)
View(train)
test<-master[-train,]$RI
boost_gb<-gbm(RI ~., data=master[train,], n.trees = 10000, interaction.depth = 20, n.cores=4)
summary(boost_gb)
boost_gb_test<-predict(boost_gb, data=master[-train,], n.trees = 10000, interaction.depth = 20, n.cores=2)
#boost_gb_test2<-predict(boost_gb, data=master, n.trees = 10000, interaction.depth = 20, n.cores=2)
View(boost_gb_test)
mean((boost_gb_test-test)^2)
plot(boost_gb_test,test)


#RandomForest ----
set.seed(1)
library(randomForest)
rr.fit <- randomForest(RI ~. ,data=master[train,], subset=, mtrain=6, importance=TRUE)
summary(rr.fit)
plot(rr.fit)
rr.predict <- predict(rr.fit, newdata = master[-train,])
View(rr.predict)
plot(rr.predict, test)
library(MASS)
lm(rr.predict~test)
abline(lm(rr.predict~test), col="red")
mean((test-rr.predict)^2)

# test prediction
test_predction <- read.csv("../test_prediction.csv")
View(test_predction)
prediction <- predict(rr.fit, newdata = test_predction)
View(prediction)
library(ggplot2)
plot(rr.predict,master[-train,]$RI)


# xgboosting
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
# converting the data to Xgboost format
x_train <-master[train,][1:18]
y_train <-master[train,][19]
x_test<-master[-train,][1:18]
y_test <-master[-train,][19]
data.train <- xgb.DMatrix(data =x_train)


data(master, package='xgboost')
xgb.fit <-  xgboost(data = master.train$RI, 
                          label = master$label, 
                          eta = 0.1,
                          max_depth = 15, 
                          nround=25, 
                          subsample = 0.5,
                          colsample_bytree = 0.5,
                          seed = 1,
                          eval_metric = "merror",
                          objective = "multi:softprob",
                          num_class = 12,
                          nthread = 3
)

# Deep learning ----
