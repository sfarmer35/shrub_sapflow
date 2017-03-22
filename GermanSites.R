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




# do sap flow calculation that excludes their data flags
  #F= (Pin-Qv-Qr)/ (4.186*Dt)
  #Would like to be new column