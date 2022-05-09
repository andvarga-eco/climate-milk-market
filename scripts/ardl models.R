here::i_am("scripts/ardl models.R")
library(here)
here()
library(dplyr)
library(dynlm)
library(ARDL)
library(urca)
library(forecast)
library(lmtest)
library(NTS)
library(ggplot2)
library(haven)



# DATA

model<-readRDS(here("data_def","model.RDS"))
modelpf<-model%>%filter(Fecha>"2012-09-01")%>%select(Fecha,mes,lpfr,spei,spi,spit)%>%na.omit()
modelpr<-model%>%filter(Fecha>"2012-09-01")%>%select(Fecha,mes,lpregr,lbono,spei,spi,spit)%>%na.omit()

#MODELO FINCA

## Uroot

acf(ts(modelpf$lpfr))
lpfr.df<-ur.df(modelpf$lpfr,type="trend",selectlags=c("AIC"),lags=12)
summary(lpfr.df)
lpfr.kpss<-ur.kpss(modelpf$lpfr,type="tau",lags="long")
summary(lpfr.kpss)
lpfr.dfgls<-ur.ers(modelpf$lpfr,model="trend",lag.max=4)
summary(lpfr.dfgls)

summary(ur.df(modelpf$spei,type="none",selectlags=c("AIC"),lags=12))
summary(ur.kpss(modelpf$spei,type="mu",lags="long"))
summary(ur.ers(modelpf$spei,model="constant",lag.max=4))


### DF rechaza nula, KPSS no rechza, DFGLS rechaza. En todos los casos se considero la tendencia
### ACF sugiere dependencia débil y componente estacional
### spei,spi,spit adf rechaza nula

### Detrend

detrend<-lm(lpfr~Fecha,data=modelpf)
lpfrdt<-detrend$residuals
modelpf<-cbind(lpfrdt,modelpf)
modelpf<-modelpf%>%mutate(pfspei=lpfrdt*spei,spei2=spei^2)
write_dta(modelpf,here("data_def","modelpf.dta"))
modelpf$mes<-as.factor(modelpf$mes)
modelpf<-modelpf%>%ts(start=c(2012,10),end=c(2020,12),frequency=12)



## ARDL 


### Lag selection


aics <- rep(0,5)
bics <- rep(0,5)
for(i in 0:4){
  lpfrdti<-dynlm(lpfrdt~L(lpfrdt,c(1:1))+L(spei,c(0:i))+season(lpfrdt),start=c(2013,2),data=modelpf)
  aics[i+1]<-AIC(lpfrdti)
  bics[i+1]<-BIC(lpfrdti)
}

### Selecciona 1,0

### Modelo: 1 p=1 y q=0 se comporta bien

lpfrardl<-dynlm(lpfrdt~L(lpfrdt,c(1:1))+L(spei,c(0:0))+season(lpfrdt),data=modelpf)
summary(lpfrardl)
acf(lpfrardl$residuals)

bgtest(lpfrardl,order=1,type="Chisq",fill=0)
bgtest(lpfrardl,order=6,type="Chisq",fill=0)
bgtest(lpfrardl,order=12,type="Chisq",fill=0)


#### Con serie en diferencias para comparar

dlpfrardl<-dynlm(d(lpfr)~L(d(lpfr),c(1,11))+L(spei,c(0:0))+season(d(lpfr)),data=modelpf)
summary(dlpfrardl)
acf(dlpfrardl$residuals)

bgtest(dlpfrardl,order=1,type="Chisq",fill=0)
bgtest(dlpfrardl,order=6,type="Chisq",fill=0)
bgtest(dlpfrardl,order=12,type="Chisq",fill=0)


#MODELO PRECIO REGULADO

acf(ts(modelpr$lpregr))
lpregr.df<-ur.df(modelpr$lpregr,type="none",selectlags=c("AIC"),lags=12)
summary(lpregr.df)
lpregr.kpss<-ur.kpss(modelpr$lpregr,type="mu",lags="long")
summary(lpregr.kpss)
lpregr.dfgls<-ur.ers(modelpr$lpregr,model="constant",lag.max=4)
summary(lpregr.dfgls)

summary(ur.df(modelpf$spit,type="none",selectlags=c("BIC")))
acf(ts(modelpf$spei))

##NO estacionaria. Tomamos primera diferencia


write_dta(modelpr,here("data_def","modelpr.dta"))
modelpr<-modelpr%>%ts(start=c(2012,10),end=c(2020,12),frequency=12)

lpregr<-modelpr[,"lpregr"]
ts.plot(diff(lpregr))
acf(diff(lpregr))
## ARDL 


### Lag selection


aics <- rep(0,5)
bics <- rep(0,5)
for(i in 0:4){
  dlpregri<-dynlm(d(lpregr)~L(d(lpregr),c(1:1))+L(spei,c(0:i))+season(d(lpregr)),start=c(2013,3),data=modelpr)
  aics[i+1]<-AIC(dlpregri)
  bics[i+1]<-BIC(dlpregri)
}

dlpregrardl<-dynlm(d(lpregr)~L(d(lpregr),c(1:1))+L(spei,c(0:0))+season(d(lpregr)),data=modelpr)
summary(dlpregrardl)
acf(dlpregrardl$residuals)

bgtest(dlpregrardl,order=1,type="Chisq",fill=0)
bgtest(dlpregrardl,order=6,type="Chisq",fill=0)
bgtest(dlpregrardl,order=12,type="Chisq",fill=0)


##Bono

lbono<-modelpr[,"lbono"]
acf(lbono)
lbono.df<-ur.df(lbono,type="none",selectlags=c("AIC"),lags=12)
summary(lbono.df)
lbono.kpss<-ur.kpss(lbono,type="mu",lags="long")
summary(lbono.kpss)
lbono.dfgls<-ur.ers(lbono,model="constant",lag.max=4)
summary(lbono.dfgls)

summary(ur.df(modelpf$spit,type="none",selectlags=c("BIC")))
acf(ts(modelpf$spei))

##NO estacionaria. Tomamos primera diferencia

ts.plot(diff(lbono))
acf(diff(lbono))
## ARDL 


### Lag selection


aics <- rep(0,5)
bics <- rep(0,5)
for(i in 0:4){
  dlbonoi<-dynlm(d(lbono)~L(d(lbono),c(1:4))+L(spei,c(0:i))+season(d(lbono)),start=c(2013,3),data=modelpr)
  aics[i+1]<-AIC(dlbonoi)
  bics[i+1]<-BIC(dlbonoi)
}

dlbonoardl<-dynlm(d(lbono)~L(d(lbono),c(1:4))+L(spei,c(0:0))+season(d(lbono)),data=modelpr)
summary(dlbonoardl)
acf(dlbonoardl$residuals)

bgtest(dlbonoardl,order=1,type="Chisq",fill=0)
bgtest(dlbonoardl,order=6,type="Chisq",fill=0)
bgtest(dlbonoardl,order=12,type="Chisq",fill=0)




# Modelo no lineal finca

ggplot(data.frame(modelpf),aes(x=spei,y=lpfrdt))+geom_point()+geom_smooth(method="loess")+
  geom_smooth(method="lm",color="red")

bdsTest(data.frame(modelpf)$lpfrdt)
lpfrdt<-ts(lpfrdt)
terasvirta.test(lpfrdt)

Tsay(data.frame(modelpf)$lpfrdt,1) # No evidencia  de no linealidad. Modelos de umbrales en Stata
                                    #no detectan umbrales
PRnd(data.frame(modelpf)$lpfrdt,m=5)
PRnd(abs(data.frame(modelpf)$lpfrdt),m=5)
# nO LINEAL REGULADO

ggplot(data.frame(modelpr),aes(x=spei,y=lbono))+geom_point()+geom_smooth(method="loess")+
  geom_smooth(method="lm",color="red")


