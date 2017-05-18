# set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\Low_Density")
library(plyr)

#input variables data
datSA<- read.csv("SA_values.csv")

#TIME
library(lubridate)
dateLD<-as.Date(datSA$TIMESTAMP_f, "%m/%d/%Y %H:%M")
datSA$DOY<-yday(dateLD)

JDay<-data.frame(JDAY=datSA[,99])
JHM<-datSA[,2]

hourD<-data.frame(JHM=JHM/100)
Hour3<-ifelse(floor(hourD$JHM)-hourD$JHM < 0, floor(hourD$JHM) + 0.5, floor(hourD$JHM))

#changing DOY to start at 5am
DOYSA<-ifelse(Hour3 < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(Hour3)

JDay$TimePlot<-JDay$JDAY+(Hour3/24)

#isolate a matrix of raw data for the sensors for sensors 1-16
C<-datSA[3:18]
B<-datSA[19:34]
A<-datSA[35:50]
Pin<-datSA[51:66]
dT<-datSA[67:82]
SA<-datSA[83:98]

#set up a dataframe of full list of doy and hour
#to help us later on in joins
datetable<-data.frame(doy=DOYSA,hour=Hour3)

Ktemp<-list()
Ktemp2<-list()
Kmin<-list()
Kshapp<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
Qv<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)

#calculate the Ksh apparent for each calculation
#then find the minumum in a day for our calculations latter
#do the 
#DG_Qv(i) = DG_Kst(i) * DG_SA(i) * (B_mv(i) - A_mv(i)) / (DG_dx(i)* 10 * 0.04)
#DG_kshapp(i) = (DG_Pin(i) - DG_Qv(i)) / C_mv(i)
  for(i in 1:16) {
    #QV
    Qv[,i]<-(0.42*SA[,i]*(B[,i]-A[,i]))/(4*10*0.04)
    #Kshapp calc
    Kshapp[,i]<-(Pin[,i]-Qv[,i])/C[,i]
    #make a temp dataframe for each sensor and omit the obs
    #with na for each sensor
    Ktemp[[i]]<-na.omit(data.frame(day=DOYSA, hour=Hour3, Ksh=Kshapp[,i]))
    # now filter any values that might be negative
    Ktemp2[[i]]<-Ktemp[[i]][which(Ktemp[[i]]$Ksh>0),]
    #grab the minimum for the day
    Kmin[[i]]<-aggregate(Ktemp2[[i]]$Ksh, by=list(Ktemp2[[i]]$day), FUN="min")
    #rename columns for each dataframe
    colnames(Kmin[[i]])<-c("doy",paste0("minKsh",i))  
  }





