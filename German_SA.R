# set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\German")
3setwd("C:\\Users\\hkropp\\Google Drive\\sapflow_sf\\German")
library(plyr)
#input variables data
datSA<-read.csv("Area_inputvalues.csv")

JDay<-datSA[3]
JHM<-datSA[4]

hourD<-JHM/100
Hour3<-ifelse(floor(hourD$JHM)-hourD$JHM < 0, floor(hourD$JHM) + 0.5, floor(hourD$JHM))

#changing DOY to start at 5am
DOYSA<-ifelse(Hour3 < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(Hour)
#isolate a matrix of raw data for the sensors for sensors 1-16
C<-datSA[5:20]
B<-datSA[21:36]
A<-datSA[37:52]
Pin<-datSA[53:68]
dT<-datSA[69:84]
SA<-datSA[85:100]

#set up a dataframe of full list of doy and hour
#to help us later on in joins
datetable<-data.frame(doy=DOYSA,hour=Hour3)




  #these values are now their own "data". What does it mean to be "data" vs "values"?
  #which do I want them to be?
  
#set up dummy objects for our for loop
Ktemp<-list()
Ktemp2<-list()
Kmin<-list()
Kshapp<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
Qv<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
#calculate the Ksh apparent for each calculation
#then find the minumum in a day for our calculations latter
#do the 
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
#add a dataframe to our list with all days and hours that we should
#have observations for
Kmin[[17]]<-datetable
#now recursively join each dataframe in our list
KshAtemp<-join_all(Kmin, by="doy", type="full")
#pull out just the Ksh for the sensors
KshA<-KshAtemp[,2:17]


#now do all the sapflow calcs

#####sensor calculations
for(i in 1:16) {

  
  }





##### Heather Old Code #####
  #DG_Pin(i) = H_V(i) * H_V(i) / DG_HR(i)
  #DG_Qv(i) = DG_Kst(i) * DG_SA(i) * (B_mv(i) - A_mv(i)) / (DG_dx(i)* 10 * 0.04)
  #DG_Qr(i) = C_mv(i) * DG_Ksh(i)
  #DG_Qf(i) = DG_Pin(i) - DG_Qv(i) - DG_Qr(i)

  #DG_kshapp(i) = (DG_Pin(i) - DG_Qv(i)) / C_mv(i)
  #DG_dT(i) = ((A_mv(i) + B_mv(i) )/2 ) * 25

#' Calculate sapflow
#DG_flow(i) = DG_Qf(i)* 3600/(DG_dT(i) * 4.186)

#if (DG_Qf(i)<0.2*DG_Pin(i) AND  DG_Qf(i)<0) Then
#DG_Status(i) = STAT_KHI
#DG_Flow(i) = 0
#Exi