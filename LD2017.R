#set working directory
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\2017\\LowDensity")

datLD<- read.csv("LDsapflow1.csv")
View(datLD)
datLD<-datLD[datLD$JDAY>171,]

#raw data matrixes
C<- datLD[,5:20]
B<- datLD[,21:36]
A<- datLD[,37:52]
Flow<- datLD[,53:68]
dT<- datLD[,69:84]
Pin<- datLD[,85:100]
Qv<- datLD[,101:116]
Qr<- datLD[,117:132]
Qf<- datLD[,133:148]

#date fix
JHM_temp<- datLD$JHM/100
Hour<- ifelse(floor(JHM_temp)-JHM_temp < 0, floor(JHM_temp) + 0.5, floor(JHM_temp) )

Timeplot<- datLD$JDAY+ Hour/24

#graph dT
for (i in 1:16){
  jpeg(file=paste0(getwd(), "\\Plots\\dT\\sensor", i, ".jpeg"), width=1500, height=1000,
       units= "px")
  plot(Timeplot, dT[,i], xlab= "DOY", ylab= "dT", type= "b",
           main=paste("sensor #", i), pch=19)
  dev.off()
}

#graph Pin
for (i in 1:16){
  jpeg(file=paste0(getwd(), "\\Plots\\Pin\\sensor", i, ".jpeg"), width=1500, height=1000,
       units= "px")
  plot(Timeplot, Pin[,i], xlab= "DOY", ylab ="Pin", type="b",
          main=paste("sensor #", i), pch=19)
  dev.off()
}

#graph Qf
for (i in 1:16) {
  jpeg(file=paste0(getwd(), "\\Plots\\Qf\\sensor", i, ".jpeg"), width= 1500, height=1000,
       units= "px")
  plot(Timeplot, Qf[,i], xlab="DOY", ylab="Qf", type= "b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

#graph Flow
for (i in 1:16){
  jpeg(file=paste0(getwd(), "\\Plots\\Flow\\Sensor", i, ".jpeg"), width=1500, height=1000,
       units= "px")
  plot(Timeplot, Flow[,i], xlab= "DOY", ylab="Flow", type= "b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
  
  