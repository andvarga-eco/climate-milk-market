library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

here::i_am("scripts/graf lluvia.R")
library(here)
here()

p80<- read_csv(here("data","Precipitaciones diarias 1980-2000.csv"))
p80<-subset(p80,select=c(CodigoEstacion,Municipio,Fecha,Valor))
p20<- read_csv(here("data","Precipitaciones 2000-2020.csv"))
p20<-subset(p20,select=c(CodigoEstacion,Municipio,Fecha,Valor))

p8020<-rbind(p80,p20)
p8020$Fecha<-as.Date(p8020$Fecha)
p8020$mes<-month(p8020$Fecha,label=FALSE)
p8020$a?o<-year(p8020$Fecha)

Fecha<-seq(as.Date("1980-01-01"),by=1,len=14610)
campo<-c()
for(i in 1:14610){
  campo[i]<-"Campo De La Cruz"
}
campo<-cbind(Fecha,campo)

candelaria<-c()
for(i in 1:14610){
  candelaria[i]<-"Candelaria"
}
candelaria<-cbind(Fecha,candelaria)

manati<-c()
for(i in 1:14610){
  manati[i]<-"Manat?"
}
manati<-cbind(Fecha,manati)

repelon<-c()
for(i in 1:14610){
  repelon[i]<-"Repel?n"
}
repelon<-cbind(Fecha,repelon)

sabana<-c()
for(i in 1:14610){
  sabana[i]<-"Sabanalarga"
}
sabana<-cbind(Fecha,sabana)

santa<-c()
for(i in 1:14610){
  santa[i]<-"Santa Lucia"
}
santa<-cbind(Fecha,santa)

suan<-c()
for(i in 1:14610){
  suan[i]<-"Suan"
}
suan<-cbind(Fecha,suan)

fechaf<-rbind(campo,candelaria,manati,repelon,sabana,santa,suan)
fechaf<-data.frame(fechaf)
fechaf$Fecha<-as.Date(as.numeric(fechaf$Fecha), origin = "1970-01-01")
fechaf$Municipio<-fechaf$campo
fechaf<-subset(fechaf,select=c("Fecha","Municipio"))
p8020f<-fechaf%>%left_join(p8020,by=c("Fecha","Municipio"))
p8020f<-p8020f[order(p8020f$Municipio,p8020f$Fecha),]
p8020f<-subset(p8020f,select=-c(CodigoEstacion))


table(p8020$Municipio)#Datos por municipio


ggplot(p8020f,aes(x=Fecha,y=Valor,col=Municipio))+geom_line()

#Diario, promedio municipios

p8020fmpio<-p8020f%>%group_by(Fecha)%>%summarise(preprom=mean(Valor,na.rm=TRUE))%>%ungroup

ggplot(p8020fmpio,aes(x=Fecha,y=preprom))+geom_line()

#Mensual
p8020fmpio$mes<-month(p8020fmpio$Fecha,label=FALSE)
p8020fmpio$a?o<-year(p8020fmpio$Fecha)
p8020ft<-p8020fmpio%>% 
  group_by(a?o, mes)%>%
  summarise(promedio=mean(preprom, na.rm = TRUE), desvia=sd(preprom, na.rm = TRUE),
            total=sum(preprom, na.rm = TRUE))%>%ungroup

p8020ft<-p8020ft%>%mutate(date=make_date(a?o,mes))
ggplot(p8020ft,aes(x=date,y=promedio))+geom_line()
ggplot(p8020ft,aes(x=date,y=total))+geom_line()


#Anual
p8020fty<-p8020fmpio%>% 
  group_by(a?o)%>%
  summarise(promedio=mean(preprom, na.rm = TRUE), desvia=sd(preprom, na.rm = TRUE),
            total=sum(preprom, na.rm = TRUE))%>%ungroup


ggplot(p8020fty,aes(x=a?o,y=promedio))+geom_line()
ggplot(p8020fty,aes(x=a?o,y=total))+geom_line()


