#ABBE no Prediction
abbe_sciglass <- read.csv("ABBE_sciglass.csv")
View(abbe_sciglass)
summary(abbe_sciglass)
attach(abbe_sciglass)
plot(abbe_sciglass$ABBENUM)

#Multi linear Regression ----
lm.fit <- lm(abbe_sciglass$ABBENUM ~., data=abbe_sciglass)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

# Random Forest ----


# Neural network ---- 

