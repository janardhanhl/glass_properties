# Generative Adversarial Network
library(sqldf)
RI <- read.csv("sciglass_glasspy_atfrc_RI.csv")
RI <- RI[,-1]
RI <- sqldf("select * from RI 
                where RefractiveIndex > 1.4 and RefractiveIndex <= 2.4 ")
attach(RI)
plot(RefractiveIndex)
names(RI)
summary(RI)
abbe <- read.csv("sciglass_glasspy_atfrc_Abbe.csv")
abbe <- abbe[,-1]
library(torch)
library(RGAN)
data <- RI
transformer <- data_transformer$new()
transformer$fit(RI)
transformed_data <- transformer$transform(RI)
par(mfrow=c(3,2))
plot(transformed_data,bty="n",
     col = viridis::viridis(2, alpha = 0.7)[1],
     pch = 19,
     xlab = "Var 1",
     ylab = "Var 2",
     main = "The Real Data",
     las = 1)
res <- gan_trainer(transformed_data,eval_dropout = TRUE, plot_progress = TRUE,
                   plot_interval = 600)


