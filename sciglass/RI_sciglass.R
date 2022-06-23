# RI Sci glass
#Multiple linear regression ----
RI_sciglass <- read.csv("RI_sciglass.csv")
linear.fit<-lm(RI ~., data=RI_sciglass)
par(mfrow=c(2,2))
plot(linear.fit)
summary(linear.fit)

# Random forest ----


# Neural network ----

