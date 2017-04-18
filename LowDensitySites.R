
#Pulled values from DGL in raw data folder

setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\Low_Density")

#input low density data
datLD<- read.csv ("Aug23_PulledValues.csv")
head (datLD)
names (datLD)

library(lubridate)

dateLD<-as.Date(datLD$FIXED_TIMESTAMP, "%m/%d/%Y %H:%M")
datLD$DOY<-yday(dateLD)

#Times into usable from doy.hod
#hourD2<-datLD$JHM/100
#datLD$Hours<- ifelse(floor(hourD2)-hourD2 < 0, floor(hourD2) + 0.5, hourD2)
datLD$TimeT<- datLD$DOY + (datLD$JHM_f/24)

head(datLD$TimeT)

#Generate pdf of dT
pdf(file="LowDensity_dT.pdf", 10, 5)
for(i in 5:20)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file="LowDensity_Pin.pdf", 10, 5)
for(i in 21:36)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file="LowDensity_Qv.pdf", 10, 5)
for(i in 37:52)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file="LowDensity_Qr.pdf", 10, 5)
for(i in 53:68)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file="LowDensity_Qf.pdf", 10, 5)
for(i in 69:84)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generatte pdf of Flow
pdf(file= "LowDensity_Flow.pdf", 10, 5)
for(i in 85:101)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab= paste(names(datA) [i]),
       lwd=1, main=paste(names(datA)[i]), type = "l")
dev.off ()
