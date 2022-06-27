RI <- read.csv("sciglass_glasspy_atfrc_RI.csv")
View(RI)
RI <- RI[,-1]
attach(RI)
plot(RefractiveIndex)
abbe <- read.csv("sciglass_glasspy_atfrc_Abbe.csv")
abbe <- abbe[,-1]
View(abbe)
plot(abbe$AbbeNumber)
par(mfrow=c(2,1))
