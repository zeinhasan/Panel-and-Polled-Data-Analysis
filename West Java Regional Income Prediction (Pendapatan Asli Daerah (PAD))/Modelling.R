#Mengimpor library yang diperlukan
library(tidyverse)
library(psych)
library(tseries)
library(plm)
library(car)


#Mengimpor data
data<-read.delim("clipboard")
head(data)

#Merapikan data
names(data)[1:6]<-c("KabKota","Tahun","PAD","PD","JP","UMK")
data<-arrange(data,arrange(data,Tahun))
data$KabKota<-as.factor(data$KabKota)
str(data)

#Pemeriksaan asumsi
##Linearitas
pairs.panels(data[,-1:-2], 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)

##Normalitas
hist(data$PAD, col='steelblue', main='Normal')
ks.test(data$PAD, 'pnorm')
shapiro.test(data$PAD)

#Membentuk model
ce=plm(PAD~PD+JP+UMK,data,model="pooling",index=c("KabKota","Tahun"))
fe=plm(PAD~PD+JP+UMK,data,model="within",index=c("KabKota","Tahun"))
re=plm(PAD~PD+JP+UMK,data,model="random",index=c("KabKota","Tahun"))

#Uji Chow
pooltest(ce,fe)

#Uji Hausmann
phtest(fe,re)
##Diperoleh model yang tepat adalah model efek tetap (FE)

#Uji Breusch-Pagan
plmtest(fe,effect="twoways",type="bp") #Uji efek kali silang maupun waktu
plmtest(fe,effect="individual",type="bp") #Uji efek kali silang
plmtest(fe,effect="time",type="bp") #Uji efek waktu

##Diperoleh model FE satu arah dengan efek kali silang

#Uji signifikansi parameter (Uji Wald)
#Model 1
m1=plm(PAD~PD+JP+UMK,data,model="within",effect="individual", index=c("KabKota","Tahun"))
summary(m1) #Jumlah penduduk tidak signifikan
#Model 2
m2=plm(PAD~PD+UMK,data,model="within",effect="individual", index=c("KabKota","Tahun"))
summary(m2)
fixef(m2, type = "level")

#Diagnostic Checking
##Uji Korelasi Serial
pdwtest(m2)
##Uji No-Multikolinearitas
pool=plm(PAD~PD+UMK,data,model="pooling",index=c("KabKota","Tahun"))
vif(pool)
##Uji Homoskedastisitas
bptest(m2)
##Heteroscedasticity robust covariance estimator
coeftest(m2,vcovHC)
##Normalitas residual
hist(residuals(m2))
ks.test(residuals(m2),'pnorm')