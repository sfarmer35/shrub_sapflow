# set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\Low_Density")
library(plyr)

#input variables data
datSA<- read.csv("SA_values1.csv")
datLA<- read.csv("LDensityLA.csv")
 
#time
library(lubridate)
dateLD<-as.Date(datSA$TIMESTAMP_f, "%m/%d/%Y %H:%M")
datSA$DOY<-yday(dateLD)

JDay<-data.frame(JDAY=datSA[,99])
JHM<-datSA[,2]

#changing DOY to start at 5am
DOYSA<-ifelse(JHM < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(JHM)

JDay$TimePlot<-JDay$JDAY+(JHM/24)

#isolate a matrix of raw data for the sensors for sensors 1-16
C<-datSA[,3:18]
B<-datSA[,19:34]
A<-datSA[,35:50]
Pin<-datSA[,51:66]
dT<-datSA[,67:82]
SA<-datSA[,83:98]

#set up a dataframe of full list of doy and hour
#to help us later on in joins
datetable<-data.frame(doy=DOYSA,hour=JHM)

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
    Ktemp[[i]]<-na.omit(data.frame(day=DOYSA, hour=JHM, Ksh=Kshapp[,i]))
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

Qr<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
Qf<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowC<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowS<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf1<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA1<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf2<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA2<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf3<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA3<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowCf4<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA4<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)

FlowCf5<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)
FlowLA5<-matrix(rep(NA,dim(SA)[1]*16), ncol=16)

Q90<-numeric(0)


#####sensor calculations
#LA in meters
for(i in 1:16) {
  Qr[,i]<- C[,i]*KshA[,i]
  Qf[,i]<- Pin[,i]-Qv[,i]-Qr[,i]
  FlowC[,i]<-ifelse(Qf[,i]<0,0,(Qf[,i]*3600)/(dT[,i]*4.186))
  #filters
  #zero
  FlowCf1[,i]<-ifelse(FlowC[,i]<0, 0,
               ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA1[,i]<-FlowCf1[,i]/datLA$LA[i]
  #10 percent pin
  FlowCf2[,i]<-ifelse(FlowC[,i]< .10*Pin[,i], 0, FlowC[,i])
                      #ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA2[,i]<-FlowCf2[,i]/datLA$LA[i]
  #15 percent pin
  FlowCf3[,i]<-ifelse(FlowC[,i]< .15*Pin[,i], 0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA3[,i]<-FlowCf3[,i]/datLA$LA[i]
  #20 percent pin
  FlowCf4[,i]<-ifelse(FlowC[,i]< .20*Pin[,i], 0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA4[,i]<-FlowCf4[,i]/datLA$LA[i]
  
  Q90[i]<-ifelse(quantile(FlowC[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowC[,i], probs=0.9, na.rm=TRUE))
  #20 percent of Pin and max filter based on quantile
  FlowCf5[,i]<-ifelse(FlowC[,i]< .20*Pin[,i], 0,
                      ifelse(FlowC[,i]>Q90[i], NA, FlowC[,i]))
  FlowLA5[,i]<-FlowCf5[,i]/datLA$LA[i]
}

#seconds
 for(i in 1:16){
   FlowS[,i]<- FlowLA5[,i]/3600
 }

#Graphs
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
    plot(seq(1:dim(FlowCf)[1]), FlowCf1[,i], xlab="time", ylab="Flow ", type="b",
         main=paste("sensor #", i), pch=19)
    dev.off()
  }
  
  #now break up the code into 2 week increments and use actual time stamp
  
  #find out how many days there are
  daysA<-data.frame(JDAY=unique(JDay$JDAY))
  daysA$dayid<-seq(1,dim(daysA)[1])
  #now plot 10 days at a time
  #there are now 57 days instead of 48
daysA$plotid<-c(rep(seq(1,5),each=10),rep(6,7))

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

#Filter Graphs_subplots
#subZero
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
#sub 10 percent of pin
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

#sub 15 percent of pin
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

#sub 20 percent of pin
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

#sub 20 percent of pin and Q90 max
for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\Filters\\sub20PinQ90\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],FlowLA5[timestamp$plotid==1,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],FlowLA5[timestamp$plotid==2,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],FlowLA5[timestamp$plotid==3,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],FlowLA5[timestamp$plotid==4,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],FlowLA5[timestamp$plotid==5,i],
       xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)      
  
  dev.off()
}

####STOMATAL CONDUCTANCE####
datMet<- read.csv("LowD_met.csv")
datP<- read.csv("Pressure.csv")

#date fixes because logger issues
dates<- data.frame(doy=datetable$doy, hour=datetable$hour)
dates$hourO<- floor(dates$hour)
dates$min<- dates$hour- dates$hourO
dates$minf1<- ifelse(dates$min>0.7, 1, dates$min)
dates$minf2<- ifelse(dates$minf1>0.2&dates$minf1<0.3, 0.50, dates$minf1)
dates$hourfix<- dates$hourO + dates$minf2
  
  
#join 
El.all<- data.frame(FlowS[,1:16], doy= dates$doy, hour=dates$hourfix)
dat.most<- join(El.all, datMet, by=c("doy","hour"), type="left")
dat.all<- join(dat.most, datP, by=c("doy"), type= "left")


#vapor pressure deficit
  #making the functions
e.sat<- function(Temp) { 0.611*exp((17.502*Temp)/(Temp+240.97)) }
vpD<- function(esat,RH) {esat*((RH/100)*esat)}

#calculations
satG<- e.sat(dat.all$temp)
dat.all$D<-vpD(satG, dat.all$RH)

#Kg 
kg.func<- function(Temp) {115.8 + (0.423*Temp)}
dat.all$Kg<- kg.func(dat.all$temp)

#conversion of transpiration to kg m-2 s-1 from g
El.kg<-dat.all[,1:16]*(1/1000)

#stomatal conductance (gs)
gs.func<- function (Kg.coeff, Elkg, Vpd, P)
{((Kg.coeff*Elkg)/ Vpd)*P}
Gs.raw<-matrix(rep(NA, dim(SA)[1]*16), ncol=16)
for (i in 1:16) {
  Gs.raw<-gs.func(dat.all$Kg, El.kg, dat.all$D, dat.all$PdayGap)
}

#conversion to mmol
mol.func<-function(Gs, temp, P) {Gs*0.446* (273/(temp+273))*(P/101.3)}
Gs.mol<- matrix(rep(NA, dim(Gs.raw)[1]*16), ncol=16)
Gs.mmol<-matrix(rep(NA, dim(Gs.raw)[1]*16),ncol=16)
for (i in 1:16) {
  Gs.mol<-mol.func(Gs.raw, dat.all$temp, dat.all$PdayGap)
  Gs.mmol<-Gs.mol*1000
}
#Gs.mmol in mmol m-2 s-2



#graphing stomatal conductance (mmol)

for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\Plots\\StomatalConductance\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
  par(mfrow=c(5,1))
  plot(timestamp$TimePlot[timestamp$plotid==1],Gs.mmol[timestamp$plotid==1,i],
       xlab="time", ylab="Gs ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==2],Gs.mmol[timestamp$plotid==2,i],
       xlab="time", ylab="Gs ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==3],Gs.mmol[timestamp$plotid==3,i],
       xlab="time", ylab="Gs ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==4],Gs.mmol[timestamp$plotid==4,i],
       xlab="time", ylab="Gs ", type="b",
       main=paste("sensor #", i), pch=19)
  plot(timestamp$TimePlot[timestamp$plotid==5],Gs.mmol[timestamp$plotid==5,i],
       xlab="time", ylab="Gs ", type="b",
       main=paste("sensor #", i), pch=19)    	
  
  dev.off()
}




