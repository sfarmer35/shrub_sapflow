#set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\German")
  #Q: ended up having to set wd by opening new project. How do I open a script without 
    #it being in a wd?
getwd()

#Input data from german sites
datA<-read.csv("Data_PulledValues.csv")
head(datA)
names(datA)
 
#Convert Times to Number Fractions (ie 1030-->10.5) use columns JDAY JHM
hourD<-datA$JHM/100
datA$Time<-ifelse(floor(hourD)-hourD < 0, floor(hourD) + 0.5, floor(hourD))

#Convert Times to be Day of Year and Hour Intervals

  #trial 1- datA$timeDM <- datA$JDAY+datA$Time
      #datA$timeDM<- NULL
  #this is wrong because day 187 at 6:00 is the same as day 188 at 5:00... Oops

  #what if we make the hours a decimal on the day!
datA$timeDM<- datA$JDAY + (datA$Time / 24)
  #learned that the data (if open in this window type) does not update automatically
  
    #Heather: How about sub-setting in the graphing process? is this possible? Future ref. 

#Generate pdf of dT
pdf(file="German_dT.pdf", 10, 5)
for(i in 5:20)
plot(datA$timeDM, datA[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
    lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file="GermanPin.pdf", 10, 5)
for(i in 21:36)
  plot(datA$timeDM, datA[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file="GermanQv.pdf", 10, 5)
for(i in 37:52)
  plot(datA$timeDM, datA[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file="GermanQr.pdf", 10, 5)
for(i in 53:68)
  plot(datA$timeDM, datA[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file="GermanQf.pdf", 10, 5)
for(i in 69:84)
  plot(datA$timeDM, datA[,i] , xlab = "Time", ylab = paste(names(datA)[i]),
       lwd=1,  main=paste(names(datA)[i]), type = "l")
dev.off ()

#Generate pdf of Flow
pdf(file="GermanFlow.pdf", 10 , 5)
for (i  in 85:100)
  plot(datA$timeDM, datA[,i], xlab= "Time", ylab = paste(names(datA)[i]), 
       lwd=1, main=paste(names(datA)[i]), type = "l")
dev.off ()

# do sap flow calculation that excludes their data flags
  #F= (Pin-Qv-Qr)/ (4.186*Dt)
  #Would like to be new column