
#Pulled values from DGL in raw data folder

setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\Low_Density")

#input low density data
datLD<- read.csv ("Aug23_PulledValues.csv")
head (datLD)
names (datLD)

#Times into usable from doy.hod
hourD2<-datLD$JHM/100
datLD$Hours<- ifelse(floor(hourD2)-hourD2 < 0, floor(hourD2) + 0.5, hourD2)
datLD$TimeT<- datLD$JDAY + (datLD$Hours/100)

#Generate pdf of dT
pdf(file="LowDensity_dT.pdf", 10, 5)
for(i in 5:20)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

  #the data is a bit funky, sensors re-started and denoted the DOY wrong
  #starts at ~170, restarts recording and jumps to ~40, restarts recording and 
  #jumps to 187
        #Heather, cut out data before 187? Or are the lower days really the 5 days missing 
        #between the first and third recording session?
  