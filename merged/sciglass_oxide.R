library(sqldf)
RI <- read.csv("sciglass_glasspy_atfrc_RI.csv")
View(RI)
RI <- RI[,-1]
RI <- sqldf("select * from RI 
                where RefractiveIndex > 1.4 and RefractiveIndex <= 2.4 ")
attach(RI)
plot(RefractiveIndex)
names(RI)
summary(RI)
abbe <- read.csv("sciglass_glasspy_atfrc_Abbe.csv")
abbe <- abbe[,-1]
View(abbe)
names(abbe)
plot(abbe$AbbeNumber)
par(mfrow=c(2,1))
plot(RefractiveIndex)
identify(RefractiveIndex)

# Multiple linear regression
multilin <- lm(RefractiveIndex ~. , data = RI)
par(mfrow=c(2,2))
plot(multilin)
summary(multilin)

# Random forest for RI fitting and prediction 
set.seed(5)
randsampl <- sample (1: nrow(RI), nrow(RI)/4)
train <- RI[-randsampl,]
test <- RI[randsampl,]$RefractiveIndex
library(randomForest)
rf.fit <- randomForest(RefractiveIndex ~., data = RI[-randsampl,], ntree=500, importance=TRUE)
rf.fit
summary(rf.fit)
plot(rf.fit)
rf.predict <- predict(rf.fit, newdata = RI[randsampl,])
plot(rf.predict,test)
library(MASS)
lm(rf.predict~0+test)
abline(lm(rf.predict~0+test), col="red")
mean((test-rf.predict)^2)
saveRDS(rf.fit, "./rf.fit.rds")

# Deep learning for RI learning and prediction 
library(keras)
library(tensorflow)
modeldl <- keras_model_sequential()

