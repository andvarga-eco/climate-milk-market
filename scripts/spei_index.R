library(SPEI)

here::i_am("scripts/spei_index.R")
library(here)
here()
library(dplyr)
library(readxl)
library(ggplot2)
sepi_atl<- read_excel(here("data_def","sepi_atl.xlsx"))
prcp8020<-readRDS(here("data_def","prcp8020.RDS"))

sepi_atl<-sepi_atl%>%filter(year>1979)%>%mutate(mes=month)%>%select(-month)
sepi_atl<-sepi_atl%>%full_join(prcp8020,by=c("year","mes"))
sepi_atl<-sepi_atl%>%na.omit()
sepi_atl$day<-01
sepi_atl$Fecha<-as.Date(with(sepi_atl,paste(year,mes,day,sep="-")),"%Y-%m-%d")
ggplot(sepi_atl,aes(x=Fecha))+geom_line(aes(y=prcp))+geom_line(aes(y=total,color="red"))

sepi_atl$pet<-thornthwaite(sepi_atl$temp, 10.4547) #latitud de Manatí
sepi_atl$bal<-sepi_atl$prcp-sepi_atl$pet
sepi_atl<-ts(sepi_atl,start=c(1980,1),end=c(2020,12),frequency=12)


spei1 <- spei(sepi_atl[,'bal'],1)
plot(spei1)


spi1 <- spi(sepi_atl[,'prcp'],1)
plot(spi1)

spi1t<-spi(sepi_atl[,'total'],1) # con la serie propia de precipiataciones
plot(spi1t)

spei<-cbind(spei1$fitted,spi1$fitted,spi1t$fitted)
speidf<-data.frame(spei)
speidf<-speidf%>%mutate(Fecha=seq(from=as.Date("1980-01-01"), to=as.Date("2020-12-31"),by="month"))
names(speidf)<-c("spei","spi","spit","Fecha")

sepi_atl<-data.frame(sepi_atl)
sepi_atl<-sepi_atl%>%mutate(Fecha=seq(from=as.Date("1980-01-01"), to=as.Date("2020-12-31"),by="month"))
speidf<-full_join(speidf,sepi_atl,by="Fecha")


saveRDS(speidf,file=here("data_def","speidf.RDS"))
