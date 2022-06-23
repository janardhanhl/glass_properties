#RI for interglad glass ----
# multiple Linear regression----
RI_interglad<-read.csv("RI_interglad.csv")
summary(RI_interglad)
head(RI_interglad)
dim(RI_interglad)
attach(RI_interglad)
plot(RI, main = " complete RI distribution")
plot(RI, PbO)
plot(RI, SiO2)
hist(RI,100 )
summary(RI_interglad)
linear.fit<-lm(RI ~., data=RI_interglad)
par(mfrow=c(2,2))
plot(linear.fit)
summary(linear.fit)
plot(RI_interglad$RI)


# Gradient boosting ----
library(gbm)
set.seed(1)
train<-sample (1: nrow (RI_interglad), nrow (RI_interglad)/2)
test<-RI_interglad[-train,]$RI
boost_gb<-gbm(RI ~., data=RI_interglad, n.trees = 10000, interaction.depth = 20, n.cores=4)
summary(boost_gb)
boost_gb_test<-predict(boost_gb, data=RI_interglad[-train,], n.trees = 10000, interaction.depth = 20, n.cores=2)
boost_gb_test2<-predict(boost_gb, data=RI_interglad, n.trees = 10000, interaction.depth = 20, n.cores=2)
View(boost_gb_test)
mean((boost_gb_test-test)^2)
plot(boost_gb_test2,RI_interglad$RI)


#RandomForest ----
set.seed(1)
library(randomForest)
rr.fit <- randomForest(RI ~. ,data= RI_interglad, subset=, mtrain=6, importance=TRUE)
summary(rr.fit)
plot(rr.fit)
rr.predict <- predict(rr.fit, newdata = RI_interglad[-train,])
View(rr.predict)
plot(rr.predict, test)
library(MASS)
lm(rr.predict~test)
abline(lm(rr.predict~test), col="red")
mean((test-rr.predict)^2)


# Bayesian additive regression ----
library(BART)
x <- RI_interglad[,1:18]
y <- RI_interglad[,'RI']
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]
bart_fit <- gbart(xtrain,ytrain,x.test=xtest)
summary(bart_fit)
plot(bart_fit)
predict.gbart<-predict(bart_fit,newdata=x[-train,])
View(predict.gbart)
plot()

# Deeplearning with Keras ----
library(keras)
library(tensorflow)
modeldl <- keras_model_sequential()


library(ggplot2)
plot(rr.predict, test)


