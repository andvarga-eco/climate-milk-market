library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

here::i_am("scripts/graf temp.R")
library(here)
here()

p80tem<- read_csv(here("data","tempmax8099.csv.csv"))
p80tem<-subset(p80tem,select=c(CodigoEstacion,Municipio,Fecha,Valor))
p20tem<- read_csv(here("data","tempmax0020.csv.csv"))
p20tem<-subset(p20tem,select=c(CodigoEstacion,Municipio,Fecha,Valor))
p8020tem<-rbind(p80tem,p20tem)
p8020tem$Fecha<-as.Date(p8020tem$Fecha)
p8020tem<-subset(p8020tem,p8020tem$CodigoEstacion!=29035120)


#fechas completas
Fecha<-seq(as.Date("1980-01-01"),by=1,len=14610)
juan<-c()
for(i in 1:14610){
  juan[i]<-"Juan De Acosta"
}
juan<-cbind(Fecha,juan)

manati<-c()
for(i in 1:14610){
  manati[i]<-"Manatí"
}
manati<-cbind(Fecha,manati)

repelon<-c()
for(i in 1:14610){
  repelon[i]<-"Repelón"
}
repelon<-cbind(Fecha,repelon)

santa<-c()
for(i in 1:14610){
  santa[i]<-"Santa Lucia"
}
santa<-cbind(Fecha,santa)

soledad<-c()
for(i in 1:14610){
  soledad[i]<-"Soledad"
}
soledad<-cbind(Fecha,soledad)

fechaf<-rbind(juan,manati,repelon,santa,soledad)
fechaf<-data.frame(fechaf)
fechaf$Fecha<-as.Date(as.numeric(fechaf$Fecha), origin = "1970-01-01")
fechaf$Municipio<-fechaf$juan
fechaf<-subset(fechaf,select=c("Fecha","Municipio"))
p8020tf<-fechaf%>%left_join(p8020tem,by=c("Fecha","Municipio"))
p8020tf<-p8020tf[order(p8020tf$Municipio,p8020tf$Fecha),]

p8020tf<-subset(p8020tf,Municipio=="Manatí" | Municipio=="Repelón" | Municipio=="Soledad"|Municipio=="Juan De Acosta")

ggplot(p8020tf,aes(x=Fecha,y=Valor, color=Municipio))+geom_line()


#Construcci?n de series mensuales siguiendo a Osborn y Jones, 2014

#Paso 1: calcular promedios mensuales por municipio. Al menos 14 observaciones por mes
p8020tf$mes<-month(p8020tf$Fecha)
p8020tf$year<-year(p8020tf$Fecha)
munmes<-p8020tf%>%group_by(Municipio,year,mes)%>%summarise(nobs=n())%>%ungroup
nmes<-na.omit(p8020tf)
nmes<-nmes%>%group_by(Municipio,year,mes)%>%summarise(nobs=n())%>%ungroup
p8020tf<-p8020tf%>%left_join(nmes,by=c("Municipio","year","mes"))

tmes<-p8020tf%>%filter(nobs>=14)%>%group_by(Municipio,year,mes)%>%summarise(mtem=mean(Valor,na.rm=TRUE))%>%ungroup
tmes<-munmes%>%left_join(tmes,by=c("Municipio","year","mes"))
tmes$day<-01
tmes$Fecha<-as.Date(with(tmes,paste(year,mes,day,sep="-")),"%Y-%m-%d")
ggplot(tmes,aes(x=Fecha,y=mtem, color=Municipio))+geom_line()

#Paso 2: calcular la media aritm?tica por mes con las observaciones disponibles
tmesag<-tmes%>%group_by(year,mes)%>%summarise(tmax=mean(mtem,na.rm=TRUE))%>%ungroup
tmesag$day<-01
tmesag$Fecha<-as.Date(with(tmesag,paste(year,mes,day,sep="-")),"%Y-%m-%d")
tmesag<-subset(tmesag,tmesag$year>1999)
save(tmesag,file="C:/Users/andre/Google Drive/Uninorte/GESTION_LACTEO_COL/paper vulnerabilidad climática/data/tmesag.Rda")
ggplot(tmesag,aes(x=Fecha,y=tmax))+geom_line()

table(tmesag$mes)


