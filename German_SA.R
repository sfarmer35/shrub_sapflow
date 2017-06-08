# set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\German")
#3setwd("C:\\Users\\hkropp\\Google Drive\\sapflow_sf\\German")

library(plyr)
#input variables data
datSA<-read.csv("Area_inputvalues.csv")
datLA<-read.csv("GermanLA.csv")

JDay<-data.frame(JDAY=datSA[,3])
JHM<-datSA[,4]

hourD<-data.frame(JHM=JHM/100)
Hour3<-ifelse(floor(hourD$JHM)-hourD$JHM < 0, floor(hourD$JHM) + 0.5, floor(hourD$JHM))

#changing DOY to start at 5am
DOYSA<-ifelse(Hour3 < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(Hour3)

JDay$TimePlot<-JDay$JDAY+(Hour3/24)

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
  
#set up dummy objects for our for loop
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
#add a dataframe to our list with all days and hours that we should
#have observations for
Kmin[[17]]<-datetable
#now recursively join each dataframe in our list
KshAtemp<-join_all(Kmin, by="doy", type="full")
#pull out just the Ksh for the sensors
KshA<-KshAtemp[,2:17]

  #DG_Qr(i) = C_mv(i) * DG_Ksh(i)
  #DG_Qf(i) = DG_Pin(i) - DG_Qv(i) - DG_Qr(i)

####HEATHER CALC CHECK#####

#' Calculate sapflow
#DG_flow(i) = DG_Qf(i)* 3600/(DG_dT(i) * 4.186)

#now do all the sapflow calcs
Qr<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
Qf<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowC<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf1<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf2<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf3<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf4<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowS<-matrix(rep(NA, dim(SA)[1]*16), ncol=16)
FlowLA1<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA2<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA3<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA4<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)

Q90<-numeric(0)


#####sensor calculations
for(i in 1:16) {
	Qr[,i]<- C[,i]*KshA[,i]
	Qf[,i]<- Pin[,i]-Qv[,i]-Qr[,i]
	FlowC[,i]<-ifelse(Qf[,i]<0,0,(Qf[,i]*3600)/(dT[,i]*4.186))
  Q90[i]<-ifelse(quantile(FlowC[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowC[,i], probs=0.9, na.rm=TRUE))
	FlowCf1[,i]<-ifelse(FlowC[,i]<0, 0,
              ifelse(FlowC[,i]>25, NA, FlowC[,i]))
	FlowLA1[,i]<-(FlowCf1[,i]/datLA$LA[i])
  #flow in seconds
  FlowS[,i]<-ifelse(Qf[,i]<0,0,(Qf[,i])/(dT[,i]*4.186))
  #filter 10 percent pin
  FlowCf2[,i]<-ifelse(FlowC[,i]<0.10*Pin[,i],0,
                ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA2[,i]<- FlowCf2[,i]/datLA$LA[i]
  #filter 15 percent pin
  FlowCf3[,i]<-ifelse(FlowC[,i]<0.15*Pin[,i],0,
               ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA3[,i]<- FlowCf3[,i]/datLA$LA[i]
  #Filter 20 percent pin
	FlowCf4[,i]<-ifelse(FlowC[,i]<0.20*Pin[,i],0,
	                    ifelse(FlowC[,i]> Q90[i], NA, FlowC[,i]))
	FlowLA4[,i]<- FlowCf4[,i]/datLA$LA[i]
  }
  
 ####SABRINA VERSION###
#dummy objects for the "for" loop
#Qr<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
#Qf<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
#Qftemp<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
#Qffix<-matrix(rep(NA, dim(SA)[1]*16), ncol=16)
#Flow<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
#FlowFix<-matrix(rep(NA, dim(SA)[1]*16), ncol=16)

#####sensor calculations#####
#for(i in 1:16) {
  #Qr
#Qr[,i]<-(C[,i]*KshA[,i])
  #Qf
#Qf[,i]<-(Pin[,i]-Qv[,i])/C[,i]
  #Qftemp
    #filtering out 0.2*pin
    #Heather says doesn't fit our data
  #Qftemp[,i]<-(ifelse( Qf[,i] < 0.2*Pin[,i], 0, Qf[,i]))
  #ifelse time, set negatives to zero, may need to filter out infinities 
  #Qffix
    #filtering out negatives
#Qffix[,i]<- (ifelse(Qf[,i]<0, 0, Qf[,i]))
  #Flow
#Flow[,i]<-(Qffix[,i]*3600)/(dT[,i]*4.186)
  
  #FlowTemp get rid of Nan?
  #FlowFix get rid of inf?
  #Heather addresses this by saying if greater than 25
  }


#Heather Graphs
for(i in 1:16){
	jpeg(file=paste0(getwd(),"\\Plots\\FlowC\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
		plot(seq(1:dim(FlowC)[1]), FlowC[,i], xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
	}
for(i in 1:16){
	jpeg(file=paste0(getwd(),"\\Plots\\Qf\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
		plot(seq(1:dim(Qf)[1]), Qf[,i], xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
	}
for(i in 1:16){
	jpeg(file=paste0(getwd(),"\\Plots\\FlowCfilter\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
		plot(seq(1:dim(FlowCf1)[1]), FlowCf1[,i], xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
	}

###Filter Graphs###
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\Zero\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA1)[1]), FlowLA1[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\10Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA2)[1]), FlowLA2[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\15Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA3)[1]), FlowLA3[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\20Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA4)[1]), FlowLA4[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

	
	
#now break up the code into 2 week increments and use actual time stamp

#find out how many days there are
daysA<-data.frame(JDAY=unique(JDay$JDAY))
daysA$dayid<-seq(1,dim(daysA)[1])
#now plot 10 days at a time
#there are 48 days
daysA$plotid<-c(rep(seq(1,4),each=10),rep(5,8))
#now join plot ID to the JDAY data frame

timestamp<-join(JDay, daysA, by="JDAY", type="left")

for(i in 1:16){

		jpeg(file=paste0(getwd(),"\\Plots\\Flowsub\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
		par(mfrow=c(5,1))
		plot(timestamp$TimePlot[timestamp$plotid==1],FlowCf1[timestamp$plotid==1,i],
		xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
		plot(timestamp$TimePlot[timestamp$plotid==2],FlowCf1[timestamp$plotid==2,i],
		xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
		plot(timestamp$TimePlot[timestamp$plotid==3],FlowCf1[timestamp$plotid==3,i],
		xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
		plot(timestamp$TimePlot[timestamp$plotid==4],FlowCf1[timestamp$plotid==4,i],
		xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)
		plot(timestamp$TimePlot[timestamp$plotid==5],FlowCf1[timestamp$plotid==5,i],
		xlab="time", ylab="Flow ", type="b",
			main=paste("sensor #", i), pch=19)			
	
	dev.off()
}

### graphing subsets of filters ###
#Zero
for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\subZero\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],FlowLA1[timestamp$plotid==1,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],FlowLA1[timestamp$plotid==2,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],FlowLA1[timestamp$plotid==3,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],FlowLA1[timestamp$plotid==4,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],FlowLA1[timestamp$plotid==5,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)			
  
  dev.off()
}
#10 percent pin
for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\sub10Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],FlowLA2[timestamp$plotid==1,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],FlowLA2[timestamp$plotid==2,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],FlowLA2[timestamp$plotid==3,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],FlowLA2[timestamp$plotid==4,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],FlowLA2[timestamp$plotid==5,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)  		
  
  dev.off()
}

#15 percent pin
for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\sub15Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],FlowLA3[timestamp$plotid==1,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],FlowLA3[timestamp$plotid==2,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],FlowLA3[timestamp$plotid==3,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],FlowLA3[timestamp$plotid==4,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],FlowLA3[timestamp$plotid==5,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)  		
  
  dev.off()
}

#20 percent pin
for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\sub20Pin\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],FlowLA4[timestamp$plotid==1,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],FlowLA4[timestamp$plotid==2,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],FlowLA4[timestamp$plotid==3,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],FlowLA4[timestamp$plotid==4,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],FlowLA4[timestamp$plotid==5,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)    	
  
  dev.off()
}


####STOMATAL CONDUCTANCE####
datMet<-read.csv("German_met.csv")
datP<-read.csv("Pressure.csv")

#time
library(lubridate)
dateG<-as.Date(datMet$Date.Time, "%d.%m.%Y %H:%M")
datMet$DOY<-yday(dateG)
datMet$JHM<-
  
  
#vapor pressure defecit
T<-datMet[2]  
RH<-datMet[3]

e.sat<- 0.611*exp((17.502*T)/(T+240.97))
vpD<- e.sat*((RH/100)*e.sat)
