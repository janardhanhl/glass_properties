#RI for interglad glass 
RI_interglad<-read.csv("RI_interglad.csv")
View(RI_interglad)
attach(RI_interglad)
plot(RI, main = " complete RI distribution")
plot(RI, PbO)
plot(RI, SiO2)
