here::i_am("scripts/graf def.R")
library(here)
here()
library(dplyr)
library(ggplot2)
library(tidyquant)
library(ggpubr)
library(readxl)

graf<-readRDS(here("data_def","speidf.RDS"))

temp<-ggplot(graf,aes(x=Fecha,y=temp))+geom_line()+theme_minimal()+
  labs(title="Temperatura",y="Temperatura (celsius)",x="")
temp

prcp<-ggplot(graf,aes(x=Fecha,y=prcp))+geom_line()+theme_minimal()+
  labs(title="Precipitación",y="Precipitación total (mm)",x="")
prcp

graf<-graf%>%mutate(z=ifelse(spei>0,spei,0))
spei<-ggplot(graf,aes(x=Fecha,y=spei))+geom_area()+theme_minimal()+
  geom_ribbon(aes(ymin=0,ymax=spei),fill="red")+
  geom_ribbon(aes(ymin=0,ymax=z),fill="blue")+
  labs(title="SPEI Index",y="SPEI",x="")
spei


graf<-graf%>%mutate(m=ifelse(spi>0,spi,0))
spi<-ggplot(graf,aes(x=Fecha,y=spi))+geom_area()+theme_minimal()+
  geom_ribbon(aes(ymin=0,ymax=spi),fill="red")+
  geom_ribbon(aes(ymin=0,ymax=m),fill="blue")+
  labs(title="SPI Index",y="SPI",x="")
spi


spei.den<-graf%>%mutate(Periodo=ifelse(year<2000,"1980-1999","2000-2020"))%>%
  ggplot(aes(x=spei,color=Periodo))+
  geom_density()+theme_minimal()+labs(y="",x="SPEI")+
  theme(legend.position="bottom")

spei.den

ggarrange(temp,prcp,spei,spei.den,nrow=2,ncol=2)

graf%>%filter(year>2000)%>%summarise(mean(temp))


####
model<-readRDS(here("data_def","model.RDS"))

pf.plot<-model%>%filter(Fecha>"2012-09-01")%>%na.omit()%>%ggplot(aes(x=Fecha))+
  geom_line(aes(y=lpfr))+geom_line(aes(y=lpregr),color="blue")+
  theme_minimal()+labs(x="",y="PF, PR")

bono.plot<-model%>%filter(Fecha>"2012-09-01")%>%na.omit()%>%ggplot(aes(x=Fecha))+
  geom_line(aes(y=lbono))+
  theme_minimal()+labs(x="",y="Bono")



spei2<-graf%>%filter(Fecha>"2012-09-01")%>%ggplot(aes(x=Fecha,y=spei))+geom_area()+theme_minimal()+
  geom_ribbon(aes(ymin=0,ymax=spei),fill="red")+
  geom_ribbon(aes(ymin=0,ymax=z),fill="blue")+
  labs(y="SPEI",x="")

ggarrange(pf.plot,bono.plot,spei2,nrow=3)



## Funciones IRF modelos ARDL

irf <- read_excel(here("data_def","irf.xlsx"))
irf.plot<-irf%>%filter(precio!="dlpreg")%>%ggplot(aes(x=step))+geom_line(aes(y=point))+
  geom_line(aes(y=lb),color="red",linetype=4)+geom_line(aes(y=ub),color="red",linetype=4)+
  facet_wrap(~label,nrow=3,scales="free_y")+theme_minimal()+
  labs(x="Steps",y="Multiplicador dinámico")
irf.plot




