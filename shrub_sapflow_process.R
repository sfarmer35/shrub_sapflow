########################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  #############################
########################  Low Density and German Site      #############################  
######################## Heather Kropp and Sabrina Farmer  #############################
########################      Last edits 02.08.2018        #############################
########################################################################################
#set my working directory
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\sapflow_process")
########################################################################################
#output datG files
########################################################################################
#packages 
library(plyr)
library(lubridate)
#######################################################################################
#plots on and off?
#######################################################################################

#German Files
datG<-read.csv("German_components.csv")
datg.SA<-read.csv("German_sapflow.csv")
datg.LA<-read.csv("German_la.csv")
datg.Met<-read.csv("German_rh.csv")


#LD Files
datLD<- read.csv ("LD_components.csv")
datld.SA<- read.csv("LD_sapflow.csv")
datld.LA<- read.csv("LD_la.csv")
datld.Met<-read.csv("LD_rh.csv")
#Metadata
datP<-read.csv("Pressure.csv")

#######################################################################################
#component check
#################################  GERMAN  ############################################

#Convert Times to Number Fractions (ie 1030-->10.5) use columns JDAY JHM
hourD<-datG$JHM/100
datG$Time<-ifelse(floor(hourD)-hourD < 0, floor(hourD) + 0.5, floor(hourD))

#Convert Times to be Day of Year and Hour Intervals
datG$timeDM<- datG$JDAY + (datG$Time / 24)

#Generate pdf of dT
pdf(file="GermandT.pdf", 10, 5)
for(i in 5:20)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file="GermanPin.pdf", 10, 5)
for(i in 21:36)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file="GermanQv.pdf", 10, 5)
for(i in 37:52)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file="GermanQr.pdf", 10, 5)
for(i in 53:68)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file="GermanQf.pdf", 10, 5)
for(i in 69:84)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Flow
pdf(file="GermanFlow.pdf", 10 , 5)
for (i  in 85:100)
  plot(datG$timeDM, datG[,i], xlab= "Time", ylab = paste(names(datG)[i]), 
       lwd=1, main=paste(names(datG)[i]), type = "l")
dev.off ()

################################ LOW DENSITY ##########################################

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
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file="LowDensity_Pin.pdf", 10, 5)
for(i in 21:36)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file="LowDensity_Qv.pdf", 10, 5)
for(i in 37:52)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file="LowDensity_Qr.pdf", 10, 5)
for(i in 53:68)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file="LowDensity_Qf.pdf", 10, 5)
for(i in 69:84)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generatte pdf of Flow
pdf(file= "LowDensity_Flow.pdf", 10, 5)
for(i in 85:101)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab= paste(names(datLD) [i]),
       lwd=1, main=paste(names(datLD)[i]), type = "l")
dev.off ()

#######################################################################################
#######                       SHRUB SAPFLOW CALCULATIONS                    ###########
#######################################################################################
#################################  GERMAN SITE  #######################################
#######################################################################################

#input variables data
JDay<-data.frame(JDAY=datg.SA[,3])
JHM<-datg.SA[,4]

hourD<-data.frame(JHM=JHM/100)
Hour3<-ifelse(floor(hourD$JHM)-hourD$JHM < 0, floor(hourD$JHM) + 0.5, floor(hourD$JHM))

#changing DOY to start at 5am
DOYSA<-ifelse(Hour3 < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(Hour3)

JDay$TimePlot<-JDay$JDAY+(Hour3/24)

#isolate a matrix of raw data for the sensors for sensors 1-16
C<-datg.SA[,5:20]
B<-datg.SA[,21:36]
A<-datg.SA[,37:52]
Pin<-datg.SA[,53:68]
dT<-datg.SA[,69:84]
SA<-datg.SA[,85:100]

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

# Calculate sapflow formula
#DG_flow(i) = DG_Qf(i)* 3600/(DG_dT(i) * 4.186)

#now do all the sapflow calcs
  #set up empty matrices
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

#####sensor calculations######
for(i in 1:16) {
  Qr[,i]<- C[,i]*KshA[,i]
  Qf[,i]<- Pin[,i]-Qv[,i]-Qr[,i]
  FlowC[,i]<-ifelse(Qf[,i]<0,0,(Qf[,i]*3600)/(dT[,i]*4.186))
  Q90[i]<-ifelse(quantile(FlowC[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowC[,i], probs=0.9, na.rm=TRUE))
  FlowCf1[,i]<-ifelse(FlowC[,i]<0, 0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA1[,i]<-(FlowCf1[,i]/datg.LA$LA[i])
  #filter 10 percent pin
  FlowCf2[,i]<-ifelse(FlowC[,i]<0.10*Pin[,i],0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA2[,i]<- FlowCf2[,i]/datg.LA$LA[i]
  #filter 15 percent pin
  FlowCf3[,i]<-ifelse(FlowC[,i]<0.15*Pin[,i],0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA3[,i]<- FlowCf3[,i]/datg.LA$LA[i]
  #Filter 20 percent pin
  FlowCf4[,i]<-ifelse(FlowC[,i]<0.20*Pin[,i],0,
                      ifelse(FlowC[,i]> Q90[i], NA, FlowC[,i]))
  FlowLA4[,i]<- FlowCf4[,i]/datg.LA$LA[i]
  
  #flow in seconds
  FlowS[,i]<- FlowLA4[,i]/3600  
}

#Graphs
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\FlowC\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowC)[1]), FlowC[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Qf\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(seq(1:dim(Qf)[1]), Qf[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\FlowCfilter\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowCf1)[1]), FlowCf1[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

###Filter Graphs###
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\Zero\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA1)[1]), FlowLA1[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\10Pin\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA2)[1]), FlowLA2[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\15Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLA3)[1]), FlowLA3[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\20Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Flowsub\\sensor", i, ".jpeg"), width=1500, 
       height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\subZero\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\sub10Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\sub15Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\Filters\\sub20Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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

############################## GERMAN STOMATAL CONDUCTANCE #######################

#time
library(lubridate)
dateG<-as.Date(datg.Met$Date.Time, "%d.%m.%Y %H:%M")
datg.Met$DOY<-yday(dateG)

#joins
datg.Met$DHM<-datg.Met$DOY+(datg.Met$HM/24)
E.all<-data.frame(FlowS[,1:16], DOY=datetable$doy, HM=datetable$hour)
dat.most<- join(E.all, datg.Met, by=c("DOY", "HM"), type="left")
dat.all<- join(dat.most, datP, by=c("DOY"), type="left")

#vapor pressure defecit
#making functions
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

#graphing stomatal conductance (mmol)

for(i in 1:16){
  
  jpeg(file=paste0(getwd(),"\\German\\Plots\\StomatalConductance\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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



#seperate by species: 1-8 salix, 9-16 alnus   ###NEEDS WORK!
#salix<- data.frame(gs=as.vector(Gs.mmol[,1:8]), DOY=rep(dat.all$DOY, times=8), Hour=rep(dat.all$HM, times=8))
#salix1<-aggregate(salix$gs, by=list(salix$DOY, salix$Hour), FUN="mean", na.action=na.omit)
#s.time<-salix1$Group.1+(salix1$Group.2/24)
#plot(s.time, salix1$x, pch=19)
  
  #should seperating by species not be in the process script?
  #should the 2017 field data be incorporated into this script?

##########################################################################################
#########################     LOW DENSITY SHRUB SAPFLOW    ###############################
##########################################################################################

#time
dateLD<-as.Date(datld.SA$TIMESTAMP_f, "%m/%d/%Y %H:%M")
datld.SA$DOY<-yday(dateLD)

JDay<-data.frame(JDAY=datld.SA[,99])
JHM<-datld.SA[,2]

#changing DOY to start at 5am
DOYSA<-ifelse(JHM < 5, JDay$JDAY - 1, JDay$JDAY)
head(JDay)
unique(JHM)

JDay$TimePlot<-JDay$JDAY+(JHM/24)

#isolate a matrix of raw data for the sensors for sensors 1-16
C<-datld.SA[,3:18]
B<-datld.SA[,19:34]
A<-datld.SA[,35:50]
Pin<-datld.SA[,51:66]
dT<-datld.SA[,67:82]
SA<-datld.SA[,83:98]

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

#setup empty matrix
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
  FlowLA1[,i]<-FlowCf1[,i]/datld.LA$LA[i]
  #10 percent pin
  FlowCf2[,i]<-ifelse(FlowC[,i]< .10*Pin[,i], 0, FlowC[,i])
  #ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA2[,i]<-FlowCf2[,i]/datld.LA$LA[i]
  #15 percent pin
  FlowCf3[,i]<-ifelse(FlowC[,i]< .15*Pin[,i], 0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA3[,i]<-FlowCf3[,i]/datld.LA$LA[i]
  #20 percent pin
  FlowCf4[,i]<-ifelse(FlowC[,i]< .20*Pin[,i], 0,
                      ifelse(FlowC[,i]>25, NA, FlowC[,i]))
  FlowLA4[,i]<-FlowCf4[,i]/datld.LA$LA[i]
  
  Q90[i]<-ifelse(quantile(FlowC[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowC[,i], probs=0.9, na.rm=TRUE))
  #20 percent of Pin and max filter based on quantile
  FlowCf5[,i]<-ifelse(FlowC[,i]< .20*Pin[,i], 0,
                      ifelse(FlowC[,i]>Q90[i], NA, FlowC[,i]))
  FlowLA5[,i]<-FlowCf5[,i]/datld.LA$LA[i]
}

#seconds
for(i in 1:16){
  FlowS[,i]<- FlowLA5[,i]/3600
}

#Graphs
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\FlowC\\sensor", i, ".jpeg"), width=1500, 
       height=1000, units="px")
  plot(seq(1:dim(FlowC)[1]), FlowC[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Qf\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(seq(1:dim(Qf)[1]), Qf[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\FlowCfilter\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowCf1)[1]), FlowCf1[,i], xlab="time", ylab="Flow ", type="b",
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Flowsub\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Filters\\subZero\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Filters\\sub10Pin\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Filters\\sub15Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Filters\\sub20Pin\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\Filters\\sub20PinQ90\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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

########################## Low Density STOMATAL CONDUCTANCE   ################################

#date fixes because logger issues
dates<- data.frame(doy=datetable$doy, hour=datetable$hour)
dates$hourO<- floor(dates$hour)
dates$min<- dates$hour- dates$hourO
dates$minf1<- ifelse(dates$min>0.7, 1, dates$min)
dates$minf2<- ifelse(dates$minf1>0.2&dates$minf1<0.3, 0.50, dates$minf1)
dates$hourfix<- dates$hourO + dates$minf2


#join 
El.all<- data.frame(FlowS[,1:16], doy= dates$doy, hour=dates$hourfix)
dat.most<- join(El.all, datld.Met, by=c("doy","hour"), type="left")
dat.all<- join(dat.most, datP, by=c("doy"), type= "left")
  #something is going wrong in this last join

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
  
  jpeg(file=paste0(getwd(),"\\LD\\Plots\\StomatalConductance\\sensor", i, ".jpeg"),
       width=1500, height=1000, units="px")
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




