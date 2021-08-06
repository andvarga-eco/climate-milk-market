library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)

finca <- read_excel("finca.xlsx")
ups <- read_excel("ups.xlsx")
load("tmesag.Rda")
finca$year<-finca$y
finca$mes<-finca$m
finca<-finca%>%left_join(tmesag,by=c("year","mes"))

fs<-subset(finca,select=c(year,mes,municipio,precio_real_,precio_nominal,precipitacion_prom,temperatura_promedio,tmax))

fs$day<-01
fs$fecha<-as.Date(with(fs,paste(year,mes,day,sep="-")),"%Y-%m-%d")

ggplot(fs,aes(x=fecha,y=precipitacion_prom,group=municipio,color=municipio))+geom_line()

ggplot(fs,aes(x=fecha,y=precio_nominal,group=municipio,color=municipio))+geom_line()
ggplot(fs,aes(x=fecha,y=precio_real_,group=municipio,color=municipio))+geom_line()
pmes_plot<-fs%>%group_by(fecha)%>%summarise(mprecio=mean(precio_real_))%>%ggplot(aes(x=fecha,y=mprecio))+
  geom_line()+
  annotate("rect", xmin=as.Date("2014-09-01"), xmax =as.Date("2016-05-01"), ymin = 800, ymax = 1200, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2019-01-01"), xmax =as.Date("2019-03-01"), ymin = 800, ymax = 1200, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2016-06-01"), xmax =as.Date("2017-01-01"), ymin = 800, ymax = 1200, alpha = .5,fill="#00AFBB")+
  annotate("rect", xmin=as.Date("2017-09-01"), xmax =as.Date("2018-05-01"), ymin = 800, ymax = 1200, alpha = .5,fill="#00AFBB")
pmes_plot

tmax_plot<-fs%>%group_by(fecha)%>%summarise(mtmax=mean(tmax,na.rm=TRUE))%>%ggplot(aes(x=fecha,y=mtmax))+
  geom_line()+
  annotate("rect", xmin=as.Date("2014-09-01"), xmax =as.Date("2016-05-01"), ymin = 30, ymax = 40, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2019-01-01"), xmax =as.Date("2019-03-01"), ymin = 30, ymax = 40, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2016-06-01"), xmax =as.Date("2017-01-01"), ymin = 30, ymax = 40, alpha = .5,fill="#00AFBB")+
  annotate("rect", xmin=as.Date("2017-09-01"), xmax =as.Date("2018-05-01"), ymin = 30, ymax = 40, alpha = .5,fill="#00AFBB")
tmax_plot

preprom_plot<-fs%>%group_by(fecha)%>%summarise(mpre=mean(precipitacion_prom,na.rm=TRUE))%>%ggplot(aes(x=fecha,y=mpre))+
  geom_line()+
  annotate("rect", xmin=as.Date("2014-09-01"), xmax =as.Date("2016-05-01"), ymin = 0, ymax = 1000, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2019-01-01"), xmax =as.Date("2019-03-01"), ymin = 0, ymax = 1000, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2016-06-01"), xmax =as.Date("2017-01-01"), ymin = 0, ymax = 1000, alpha = .5,fill="#00AFBB")+
  annotate("rect", xmin=as.Date("2017-09-01"), xmax =as.Date("2018-05-01"), ymin = 0, ymax = 1000, alpha = .5,fill="#00AFBB")
preprom_plot


mp<-fs%>%filter(municipio=="Manatí")%>%ggplot(aes(x=fecha,y=precio_real_))+geom_line()
mt<-fs%>%filter(municipio=="Manatí")%>%ggplot(aes(x=fecha,y=tmax))+geom_line()
mpr<-fs%>%filter(municipio=="Manatí")%>%ggplot(aes(x=fecha,y=precipitacion_prom))+geom_line()

ggarrange(mp,mt,mpr,ncol=1,nrow=3)




ups$day<-01
ups$fecha<-as.Date(with(ups,paste(y,m,day,sep="-")),"%Y-%m-%d")
ups$bonr<-ups$precio_real_con_bonifvol-ups$precio_real_sin_bonifvol
ggplot(ups,aes(x=fecha))+geom_line(aes(y=precio_real_finca))+geom_line(aes(y=bonr))
ggplot(ups,aes(x=fecha,y=bonr))+geom_line()+
  annotate("rect", xmin=as.Date("2015-04-01"), xmax =as.Date("2016-05-01"), ymin = 95, ymax = 305, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2019-01-01"), xmax =as.Date("2019-03-01"), ymin = 95, ymax = 305, alpha = .5,fill="red")+
  annotate("rect", xmin=as.Date("2013-04-01"), xmax =as.Date("2013-07-01"), ymin = 95, ymax = 305, alpha = .5,fill="#00AFBB")+
  annotate("rect", xmin=as.Date("2016-06-01"), xmax =as.Date("2017-01-01"), ymin = 95, ymax = 305, alpha = .5,fill="#00AFBB")+
  annotate("rect", xmin=as.Date("2017-09-01"), xmax =as.Date("2018-05-01"), ymin = 95, ymax = 305, alpha = .5,fill="#00AFBB")

bonr_plot<-ggplot(ups,aes(x=fecha,y=bonr))+geom_line()
pre_plot<-ggplot(ups,aes(x=fecha,y=precipitacion_promedio))+geom_line()
ggarrange(bonr_plot,pre_plot,nrow=2)