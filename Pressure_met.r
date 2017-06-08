############################################################
##### Reads and organizes pressure data from ###############
##### the Cherskii airport. Converts to KPa  ###############
############################################################



setwd("c:\\Users\\hkropp\\Google Drive\\sapflow_sf\\met")
datR<-read.table("airport.csv.csv", sep=";", header=TRUE)

#convert pressure from mm per HG to Kpa
Pnew<-datR$Po*(1/7.501)

#now convert dates
library(lubridate)
timeR<-gsub(".","/",)

dateR<-as.Date(row.names(datR), "%d.%m.%Y %H:%M")
doyR<-yday(dateR)
YearR<-year(dateR)

Pday<-aggregate(Pnew, by=list(doyR, YearR), FUN="mean", na.omit=TRUE)

Pday$PdayGap<-ifelse(is.na(Pday$x),(Pday$x[-1]+Pday$x[+1])/2,Pday$x)
colnames(Pday)[1:3]<-c("doy","year","Pkpa")
write.table(Pday,"Pressure.csv", sep=",",row.names=FALSE)
