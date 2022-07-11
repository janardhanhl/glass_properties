# Training model for RI prediction ----
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
rf.fit <- randomForest(RefractiveIndex ~., data = RI[-randsampl,], ntree=1000, importance=TRUE)
rf.fit
summary(rf.fit)
plot(rf.fit)
rf.predict <- predict(rf.fit, newdata = RI[randsampl,])
plot(rf.predict,test)
library(MASS)
lm(rf.predict~test)
abline(lm(rf.predict~test), col="red")
mean((test-rf.predict)^2)
saveRDS(rf.fit, "./rf.fit.rds")
rf.fit <- readRDS("rf.fit.rds")
print(rf.fit)
rf.predict <- predict(rf.fit, newdata = RI[randsampl,]) 
# load Random forest library before running rf predict from saved model
plot(test, rf.predict)
lm(test~rf.predict)
abline(lm(test~rf.predict), col="red")
mean((rf.predict-test)^2)

# Deep learning for RI learning and prediction 
library(reticulate)
library(keras)
library(tensorflow)
x <- model.matrix(RefractiveIndex ~. -1, data=RI) %>% scale()
y <- RI$RefractiveIndex
modeldl <- keras_model_sequential() %>% 
  layer_dense(units = 50, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 1)

modeldl %>% compile(loss="mse", optimizer="adam",
                    metrics = c("mean_absolute_error", "accuracy"))
history <- modeldl %>% fit(
  x[-randsampl,],y[-randsampl], epochs=1500, batch_size=32,
  validation_data=list(x[randsampl,], y[randsampl] )
)
View(history)
plot(history)
predected <- predict(modeldl, x[randsampl,])
plot(predected,test)
lm(predected~test)
abline(lm(predected~test), col='red')
# random forest gave better results compared to this NN Model.

# NN model 2
modeldl2 <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 128, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 1)

modeldl2 %>% compile(loss="mse", optimizer="adam",
                    metrics = c("mean_absolute_error", "accuracy"))
history2 <- modeldl2 %>% fit(
  x[-randsampl,],y[-randsampl], epochs=700, batch_size=32,
  validation_data=list(x[randsampl,], y[randsampl] )
)
predected2 <- predict(modeldl2, x[randsampl,])
plot(predected2,test)
lm(predected2~test)
abline(lm(predected~test), col='red')
mean((predected2-test)^2)

save_model_tf(modeldl2, "modeldl2") # saving tf model
read_model <- load_model_tf("modeldl2") # loading tf model 
reloaded2 <- predict(read_model, x[randsampl,])


# NN model 3
modeldl3 <- keras_model_sequential() %>% 
  layer_dense(units = 138, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 68, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 1)

modeldl3 %>% compile(loss="mse", optimizer="adam",
                     metrics = c("mean_absolute_error", "accuracy"))
history3 <- modeldl3 %>% fit(
  x[-randsampl,],y[-randsampl], epochs=1500, batch_size=64,
  validation_data=list(x[randsampl,], y[randsampl] )
)
history3
predected3 <- predict(modeldl3, x[randsampl,])
plot(test,predected3)
lm(test~predected3)
abline(lm(test~predected3), col='red')
mean((predected3-test)^2)
save_model_tf(modeldl3, "modeldl3") # saving tf model
read_model3 <- load_model_tf("modeldl3") # loading tf model

 # NN model 4
modeldl4 <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 138, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 68, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 1)

modeldl4 %>% compile(loss="mse", optimizer="adam",
                     metrics = c("mean_absolute_error"))
history4 <- modeldl4 %>% fit(
  x[-randsampl,],y[-randsampl], epochs=700, batch_size=128,
  validation_data=list(x[randsampl,], y[randsampl] )
)
history4
predected4 <- predict(modeldl4, x[randsampl,])
plot(test,predected4)
lm(test~predected4)
abline(lm(test~predected4), col='red')
mean((predected4-test)^2)
save_model_tf(modeldl4, "modeldl4") # saving tf model
read_model <- load_model_tf("modeldl4") # loading tf model 
reloaded <- predict(read_model, x[randsampl,])


# Training model for Abbe prediction ----

# Random forest for Abbe fitting and prediction 
set.seed(10)
randsampl <- sample (1: nrow(abbe), nrow(abbe)/4)
train <- abbe[-randsampl,]
test <- abbe[randsampl,]$AbbeNumber
library(randomForest)
rf.fit <- randomForest(AbbeNumber ~., data = abbe[-randsampl,], ntree=1000, importance=TRUE)
rf.fit
summary(rf.fit)
plot(rf.fit)
rf.predict <- predict(rf.fit, newdata = abbe[randsampl,])
plot(test,rf.predict)
library(MASS)
lm(test~rf.predict)
abline(lm(test~rf.predict), col="red")
mean((test-rf.predict)^2)
saveRDS(rf.fit, "./rf.fit.abbe.rds")
rf.fit <- readRDS("rf.fit.abbe.rds")
print(rf.fit)
rf.predict <- predict(rf.fit, newdata = abbe[randsampl,]) 
# load Random forest library before running rf predict from saved model
plot(test, rf.predict)
lm(test~rf.predict)
abline(lm(test~rf.predict), col="red")
mean((rf.predict-test)^2)


# NN model for abbe prediction
library(reticulate)
library(keras)
library(tensorflow)
attach(abbe)
x <- model.matrix(AbbeNumber ~. -1, data=abbe) %>% scale()
y <- abbe$AbbeNumber

modeldlabbe <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 138, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 68, activation = "relu", input_shape=ncol(x)) %>%
  layer_dropout(rate=0.1) %>% 
  layer_dense(units = 1)

modeldlabbe %>% compile(loss="mse", optimizer="adam" ,
                     metrics = c("mean_absolute_error"))
historyabbe <- modeldlabbe %>% fit(
  x[-randsampl,],y[-randsampl], epochs=1000, batch_size=128,
  validation_data=list(x[randsampl,], y[randsampl] )
)
historyabbe
predectedabbe <- predict(modeldlabbe, x[randsampl,])
plot(test,predectedabbe)
lm(test~predectedabbe)
abline(lm(test~predectedabbe), col='red')
mean((test-predectedabbe)^2)
save_model_tf(modeldlabbe, "modeldlabbe") # saving tf model
read_model <- load_model_tf("modeldlabbe") # loading tf model 
reloaded <- predict(read_model, x[randsampl,])


# Using pre trained NN models for prediction with the input files
read_model <- load_model_tf("modeldl4") # loading tf model )
pre=read.csv("prediction_upload_file.csv")
x <- model.matrix( ~. -1, data=pre)
reloaded <- predict(read_model, x[])




