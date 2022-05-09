#Ajuste de las series de precipitaciones diarias
#Candelaria, Juan de Acosta, Manatí, Ponedera, Sabanalarga

library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)

here::i_am("scripts/prcp_data.R")
library(here)
here()

p80<- read_csv(here("data_def","pluvio 1980-1999.csv")) #01/01/1980-31/12/1999
p80<-subset(p80,select=c(CodigoEstacion,Municipio,Fecha,Valor))
p20<- read_csv(here("data_def","pluvio 2000-2021.csv")) #01/01/2000-02/02/2022
p20<-subset(p20,select=c(CodigoEstacion,Municipio,Fecha,Valor))

p8020<-rbind(p80,p20)
p8020$Fecha<-as.Date(p8020$Fecha)
p8020$mes<-month(p8020$Fecha,label=FALSE)
p8020$año<-year(p8020$Fecha)
p8020<-p8020%>%filter(año<2022)


Fecha<-seq(from=as.Date("1980-01-01"), to=as.Date("2021-12-31"),by=1)

candelaria<-c()
for(i in 1:15341){
  candelaria[i]<-"Candelaria"
}
candelaria<-cbind(Fecha,candelaria)

juan<-c()
for(i in 1:15341){
  juan[i]<-"Juan De Acosta"
}
juan<-cbind(Fecha,juan)

manati<-c()
for(i in 1:15341){
  manati[i]<-"Manatí"
}
manati<-cbind(Fecha,manati)

ponedera<-c()
for(i in 1:15341){
  ponedera[i]<-"Ponedera"
}
ponedera<-cbind(Fecha,ponedera)

sab<-c()
for(i in 1:15341){
  sab[i]<-"Sabanalarga"
}
sab<-cbind(Fecha,sab)

fechaf<-rbind(candelaria,juan,manati,ponedera,sab)
fechaf<-data.frame(fechaf)
fechaf$Fecha<-as.Date(as.numeric(fechaf$Fecha), origin = "1970-01-01")
fechaf$Municipio<-fechaf$candelaria
fechaf<-subset(fechaf,select=c("Fecha","Municipio"))
p8020f<-fechaf%>%left_join(p8020,by=c("Fecha","Municipio"))
p8020f<-p8020f[order(p8020f$Municipio,p8020f$Fecha),]
p8020f<-subset(p8020f,select=-c(CodigoEstacion))

p8020f%>%group_by(Municipio)%>%summarise(sum(is.na(Valor)))%>%ungroup # NA por municipio

ggplot(p8020f,aes(x=Fecha,y=Valor,col=Municipio))+geom_line()

#Diario, promedio municipios

p8020fmpio<-p8020f%>%group_by(Fecha)%>%summarise(preprom=mean(Valor,na.rm=TRUE))%>%ungroup

ggplot(p8020fmpio,aes(x=Fecha,y=preprom))+geom_line()

#Mensual

p8020fmpio$mes<-month(p8020fmpio$Fecha,label=FALSE)
p8020fmpio$year<-year(p8020fmpio$Fecha)
p8020ft<-p8020fmpio%>% 
  group_by(year, mes)%>%
  summarise(promedio=mean(preprom, na.rm = TRUE), desvia=sd(preprom, na.rm = TRUE),
            total=sum(preprom, na.rm = TRUE))%>%ungroup

p8020ft<-p8020ft%>%mutate(date=make_date(year,mes))
ggplot(p8020ft,aes(x=date,y=total))+geom_area()

#Promedio por mes 1980-2000

avgmes8020<-p8020ft%>%group_by(mes)%>%filter(year<2001)%>%summarise(avg=mean(total))
p8020ft<-left_join(p8020ft,avgmes8020,by="mes")
p8020ft$desvmes<-p8020ft$total-p8020ft$avg

p8020ft%>%mutate(desvmesh=desvmes/100)%>%filter(year>2000)%>%ggplot(aes(x=date))+geom_area(aes(y=desvmesh),fill="blue")+
  geom_vline(xintercept=0)+theme_minimal()


#Data para pegar con temperatura

p8020ft<-p8020ft%>%select(year,mes,date,total)
saveRDS(p8020ft,file=here("data_def","prcp8020.RDS"))

