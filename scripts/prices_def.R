
here::i_am("scripts/prices_def.R")
library(here)
here()
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(lubridate)
precios<- read_excel(here("data_def","precios.xlsx"))
precios<-precios%>%mutate(Fecha=as.Date(Fecha))
speidf<-readRDS(here("data_def","speidf.RDS"))
speidf<-speidf%>%filter(Fecha>="2008-01-01")

precios<-precios%>%full_join(speidf,by="Fecha")

precios<-precios%>%mutate(pfr=precio_finca*100/ipp,lpfr=log(pfr),pregr=precio_reg*100/ipp,
                          lpregr=log(pregr), bonor=bono*100/ipp,lbono=log(bonor))

pf<-precios%>%na.omit()%>%ggplot(aes(x=Fecha,y=precio_finca))+geom_line()
pfr<-precios%>%na.omit()%>%ggplot(aes(x=Fecha,y=log(pfr)))+geom_line()
pregr<-ggplot(precios,aes(x=Fecha,y=lpregr))+geom_line()
spei<-ggplot(precios,aes(x=Fecha,y=spei))+geom_col()+geom_hline(yintercept=0)
pr<-ggplot(precios,aes(x=Fecha,y=precio_reg))+geom_line()
bono<-ggplot(precios,aes(x=Fecha,y=bono))+geom_line()
bonor<-ggplot(precios,aes(x=Fecha,y=lbono))+geom_line()
ipp<-ggplot(precios,aes(x=Fecha,y=ipp))+geom_line()



ggarrange(ipp,pf,pr,bono,spei,nrow=5)
ggarrange(pfr,pregr,bonor,spei,nrow=4)

saveRDS(precios,file=here("data_def","model.RDS"))


