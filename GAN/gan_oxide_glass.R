# Generative Adversarial Network
setwd("E:/J-OCTA/cases/VSSC/ML_data_set/GAN")
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
device <- ("cpu")
res <- gan_trainer(transformed_data,eval_dropout = TRUE, plot_progress = TRUE,
                   plot_interval = 300, device=device, epochs = 2)
par(mfrow = c(1, 2))
noise_vector <- torch::torch_randn(c(nrow(transformed_data), 2))$to(device = device)
synth_data_dropout <- expert_sample_synthetic_data(res$generator, noise_vector, eval_dropout = TRUE)
GAN_update_plot(data = transformed_data, synth_data = synth_data_dropout, main = "With dropout")
synth_data_no_dropout <- expert_sample_synthetic_data(res$generator, noise_vector,device, eval_dropout = F)
GAN_update_plot(data = transformed_data, synth_data = synth_data_no_dropout, main = "Without dropout")

res_cont <- gan_trainer(transformed_data,
                        generator = res$generator,
                        discriminator = res$discriminator,
                        generator_optimizer = res$generator_optimizer,
                        discriminator_optimizer = res$discriminator_optimizer,
                        epochs = 10
)
res

