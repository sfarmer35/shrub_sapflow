########################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  #############################
########################  Low Density and German Site      #############################  
######################## Heather Kropp and Sabrina Farmer  #############################
########################      Last edits 02.08.2018        #############################
########################################################################################
#set my directories
datadir<-"z:\\data_repo\\field_data\\shrub_sapflow_viper"
plotdir<-"z:\\student_research\\farmer\\diagPlots"
########################################################################################
#turn on plotting
#1 = make plots, 0 = don't make plots
plotcheck<-1

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
datG<-read.csv(paste0(datadir, "\\German_components.csv"))
datg.SA<-read.csv(paste0(datadir,"\\German_sapflow.csv"))
datg.LA<-read.csv(paste0(datadir,"\\German_la.csv"))
datg.Met<-read.csv(paste0(datadir,"\\German_rh.csv"))


#LD Files
datLD<- read.csv (paste0(datadir,"\\LD_components.csv"))
datld.SA<- read.csv(paste0(datadir,"\\LD_sapflow.csv"))
datld.LA<- read.csv(paste0(datadir,"\\LD_la.csv"))
datld.Met<-read.csv(paste0(datadir,"\\LD_rh.csv"))
#Metadata
datP<-read.csv(paste0(datadir,"\\Pressure.csv"))

#######################################################################################
#time conversion
#################################  GERMAN  ############################################

#Convert Times to Number Fractions (ie 1030-->10.5) use columns JDAY JHM
hourD<-datG$JHM/100
datG$Time<-ifelse(floor(hourD)-hourD < 0, floor(hourD) + 0.5, floor(hourD))

#Convert Times to be Day of Year and Hour Intervals
datG$timeDM<- datG$JDAY + (datG$Time / 24)

################################ LOW DENSITY ##########################################

dateLD<-as.Date(datLD$FIXED_TIMESTAMP, "%m/%d/%Y %H:%M")
datLD$DOY<-yday(dateLD)

#Times into usable from doy.hod
#hourD2<-datLD$JHM/100
#datLD$Hours<- ifelse(floor(hourD2)-hourD2 < 0, floor(hourD2) + 0.5, hourD2)
datLD$TimeT<- datLD$DOY + (datLD$JHM_f/24)

head(datLD$TimeT)

#######################################################################################
#component check

if(plotcheck==1) {
#################################  GERMAN  ############################################
#Generate pdf of dT
pdf(file=paste0(plotdir, "\\GermandT.pdf"), 10, 5)
for(i in 5:20)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file=paste0(plotdir, "\\GermanPin.pdf"), 10, 5)
for(i in 21:36)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file=paste0(plotdir, "\\GermanQv.pdf"), 10, 5)
for(i in 37:52)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file=paste0(plotdir, "\\GermanQr.pdf"), 10, 5)
for(i in 53:68)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file= paste0(plotdir, "\\GermanQf.pdf"), 10, 5)
for(i in 69:84)
  plot(datG$timeDM, datG[,i] , xlab = "Time", ylab = paste(names(datG)[i]),
       lwd=1,  main=paste(names(datG)[i]), type = "l")
dev.off ()

#Generate pdf of Flow
pdf(file=paste0(plotdir, "\\GermanFlow.pdf"), 10 , 5)
for (i  in 85:100)
  plot(datG$timeDM, datG[,i], xlab= "Time", ylab = paste(names(datG)[i]), 
       lwd=1, main=paste(names(datG)[i]), type = "l")
dev.off ()

################################ LOW DENSITY ##########################################

#Generate pdf of dT
pdf(file= paste0(plotdir, "\\LowDensity_dT.pdf"), 10, 5)
for(i in 5:20)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Pin
pdf(file=paste0(plotdir, "\\LowDensity_Pin.pdf"), 10, 5)
for(i in 21:36)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qv
pdf(file= paste0(plotdir, "\\LowDensity_Qv.pdf"), 10, 5)
for(i in 37:52)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qr
pdf(file= paste0(plotdir, "\\LowDensity_Qr.pdf"), 10, 5)
for(i in 53:68)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Qf
pdf(file=paste0(plotdir, "\\LowDensity_Qf.pdf"), 10, 5)
for(i in 69:84)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab = paste(names(datLD)[i]),
       lwd=1,  main=paste(names(datLD)[i]), type = "l")
dev.off ()

#Generate pdf of Flow
pdf(file= paste0(plotdir, "\\LowDensity_Flow.pdf"), 10, 5)
for(i in 85:101)
  plot(datLD$TimeT, datLD[,i] , xlab = "Time", ylab= paste(names(datLD) [i]),
       lwd=1, main=paste(names(datLD)[i]), type = "l")
dev.off ()

}
#######################################################################################
#######                       SHRUB SAPFLOW CALCULATIONS                    ###########
#######################################################################################
#################################  GERMAN SITE  #######################################
#######################################################################################

#time, for ld
dateLD<-as.Date(datld.SA$TIMESTAMP_f, "%m/%d/%Y %H:%M")
datld.SA$DOY<-yday(dateLD)

#input variables data
#pull out day data
JDayG<-data.frame(JDAY=datg.SA[,3])
JHMG<-datg.SA[,4]

JDayL<-data.frame(JDAY=datld.SA[,99])
JHML<-datld.SA[,2]

#convert hours to minutes for german site
hourDG<-data.frame(JHMG=JHMG/100)
Hour3G<-ifelse(floor(hourDG$JHMG)-hourDG$JHMG < 0, floor(hourDG$JHMG) + 0.5, floor(hourDG$JHMG))

#changing DOY to start at 5am
DOYSAG<-ifelse(Hour3G < 5, JDayG$JDAY - 1, JDayG$JDAY)
DOYSAL<-ifelse(JHML < 5, JDayL$JDAY - 1, JDayL$JDAY)


JDayG$TimePlot<-JDayG$JDAY+(Hour3G/24)

JDayL$TimePlot<-JDayL$JDAY+(JHML/24)


#isolate a matrix of raw data for the sensors for sensors 1-16
CG<-datg.SA[,5:20]
BG<-datg.SA[,21:36]
AG<-datg.SA[,37:52]
PinG<-datg.SA[,53:68]
dTG<-datg.SA[,69:84]
SAG<-datg.SA[,85:100]

CL<-datld.SA[,3:18]
BL<-datld.SA[,19:34]
AL<-datld.SA[,35:50]
PinL<-datld.SA[,51:66]
dTL<-datld.SA[,67:82]
SAL<-datld.SA[,83:98]

#set up a dataframe of full list of doy and hour
#to help us later on in joins
datetableG<-data.frame(doy=DOYSAG,hour=Hour3G)

datetableL<-data.frame(doy=DOYSAL,hour=JHML)

#set up dummy objects for our for loop
KtempG<-list()
Ktemp2G<-list()
KminG<-list()
KshappG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
QvG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)

KtempL<-list()
Ktemp2L<-list()
KminL<-list()
KshappL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
QvL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)

#calculate the Ksh apparent for each calculation
#then find the minumum in a day for our calculations latter
#do the 
#DG_Qv(i) = DG_Kst(i) * DG_SA(i) * (B_mv(i) - A_mv(i)) / (DG_dx(i)* 10 * 0.04)
#DG_kshapp(i) = (DG_Pin(i) - DG_Qv(i)) / C_mv(i)
for(i in 1:16) {
  #QV
  QvG[,i]<-(0.42*SAG[,i]*(BG[,i]-AG[,i]))/(4*10*0.04)
  #Kshapp calc
  KshappG[,i]<-(PinG[,i]-QvG[,i])/CG[,i]
  #make a temp dataframe for each sensor and omit the obs
  #with na for each sensor
  KtempG[[i]]<-na.omit(data.frame(day=DOYSAG, hour=Hour3G, Ksh=KshappG[,i]))
  # now filter any values that might be negative
  Ktemp2G[[i]]<-KtempG[[i]][which(KtempG[[i]]$Ksh>0),]
  #grab the minimum for the day
  KminG[[i]]<-aggregate(Ktemp2G[[i]]$Ksh, by=list(Ktemp2G[[i]]$day), FUN="min")
  #rename columns for each dataframe
  colnames(KminG[[i]])<-c("doy",paste0("minKsh",i))  
  #QV
  QvL[,i]<-(0.42*SAL[,i]*(BL[,i]-AL[,i]))/(4*10*0.04)
  #Kshapp calc
  KshappL[,i]<-(PinL[,i]-QvL[,i])/CL[,i]
  #make a temp dataframe for each sensor and omit the obs
  #with na for each sensor
  KtempL[[i]]<-na.omit(data.frame(day=DOYSAL, hour=JHML, Ksh=KshappL[,i]))
  # now filter any values that might be negative
  Ktemp2L[[i]]<-KtempL[[i]][which(KtempL[[i]]$Ksh>0),]
  #grab the minimum for the day
  KminL[[i]]<-aggregate(Ktemp2L[[i]]$Ksh, by=list(Ktemp2L[[i]]$day), FUN="min")
  #rename columns for each dataframe
  colnames(KminL[[i]])<-c("doy",paste0("minKsh",i))  
}

#add a dataframe to our list with all days and hours that we should
#have observations for
KminG[[17]]<-datetableG
#now recursively join each dataframe in our list
KshAtempG<-join_all(KminG, by="doy", type="full")
#pull out just the Ksh for the sensors
KshAG<-KshAtempG[,2:17]

KminL[[17]]<-datetableL
#now recursively join each dataframe in our list
KshAtempL<-join_all(KminL, by="doy", type="full")
#pull out just the Ksh for the sensors
KshAL<-KshAtempL[,2:17]


#DG_Qr(i) = C_mv(i) * DG_Ksh(i)
#DG_Qf(i) = DG_Pin(i) - DG_Qv(i) - DG_Qr(i)

# Calculate sapflow formula
#DG_flow(i) = DG_Qf(i)* 3600/(DG_dT(i) * 4.186)

#now do all the sapflow calcs
  #set up empty matrices
QrG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
QfG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
FlowUCG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
FlowCG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
FlowLAG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
FlowSG<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
Q90G<-numeric(0)

QrL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
QfL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowUCL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowCL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowLAL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowSL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
Q90L<-numeric(0)

#####sensor calculations######
for(i in 1:16) {
  QrG[,i]<- CG[,i]*KshAG[,i]
  QfG[,i]<- PinG[,i]-QvG[,i]-QrG[,i]
  FlowUCG[,i]<-ifelse(QfG[,i]<0,0,(QfG[,i]*3600)/(dTG[,i]*4.186))
  #for sensors with many spikes setting a limit for quantile @ 150
  Q90G[i]<-ifelse(quantile(FlowUCG[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowUCG[,i], probs=0.9, na.rm=TRUE))
   #Filter 20 percent pin
  FlowCG[,i]<-ifelse(FlowUCG[,i]<0.20*PinG[,i],0,
                      ifelse(FlowUCG[,i]> Q90G[i], NA, FlowUCG[,i]))
  FlowLAG[,i]<- FlowCG[,i]/datg.LA$LA[i]
  
  #seconds for stomatal conductance
  FlowSG[,i]<- FlowLAG[,i]/3600
  
  QrL[,i]<- CL[,i]*KshAL[,i]
  QfL[,i]<- PinL[,i]-QvL[,i]-QrL[,i]
  FlowUCL[,i]<-ifelse(QfL[,i]<0,0,(QfL[,i]*3600)/(dTL[,i]*4.186))
  
  #for sensors with many spikes setting a limit for quantile @ 150
  Q90L[i]<-ifelse(quantile(FlowUCL[,i], probs=0.9, na.rm=TRUE)>150, 150, 
                 quantile(FlowUCL[,i], probs=0.9, na.rm=TRUE))
  #Filter 20 percent pin
  FlowCL[,i]<-ifelse(FlowUCL[,i]<0.20*PinL[,i],0,
                    ifelse(FlowUCL[,i]> Q90L[i], NA, FlowUCL[,i]))
  FlowLAL[,i]<- FlowCL[,i]/datg.LA$LA[i]
  #seconds for stomatal conductance
  FlowSL[,i]<- FlowLAL[,i]/3600
    
  }

if(plotcheck==1){
  
#Graphs
for(i in 1:16){
  jpeg(file=paste0(plotdir, "\\Flow\\German\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLAG)[1]), FlowLAG[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir, "\\Flow\\LD\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowLAL)[1]), FlowLAL[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Qf\\German\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(seq(1:dim(QfG)[1]), QfG[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Qf\\LD\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(seq(1:dim(QfL)[1]), QfL[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
}

##############################  STOMATAL CONDUCTANCE ##############################

#date adjustments
dateG<-as.Date(datg.Met$Date.Time, "%d.%m.%Y %H:%M")
datg.Met$DOY<-yday(dateG)

  #ld logger issues
datesL<- data.frame(doy=datetable$doy, hour=datetable$hour)
datesL$hourO<- floor(datesL$hour)
datesL$min<- datesL$hour- datesL$hourO
datesL$minf1<- ifelse(datesL$min>0.7, 1, datesL$min)
datesL$minf2<- ifelse(datesL$minf1>0.2&datesL$minf1<0.3, 0.50, datesL$minf1)
datesL$hourfix<- datesL$hourO + datesL$minf2

#joins
datg.Met$DHM<-datg.Met$DOY+(datg.Met$HM/24)
E.allG<-data.frame(FlowSG[,1:16], DOY=datetableG$doy, HM=datetableG$hour)
dat.mostG<- join(E.allG, datg.Met, by=c("DOY", "HM"), type="left")
dat.allG<- join(dat.mostG, datP, by=c("DOY"), type="left")
  #re-made flow in seconds 
El.allL<- data.frame(FlowSL[,1:16], doy= datesL$doy, hour=datesL$hourfix)
dat.mostL<- join(El.allL, datld.Met, by=c("doy","hour"), type="left")
dat.allL<- join(dat.mostL, datP, by=c("doy"), type= "left")
#something is going wrong in this last join


#vapor pressure defecit
#making functions
e.sat<- function(Temp) { 0.611*exp((17.502*Temp)/(Temp+240.97)) }
vpD<- function(esat,RH) {esat*((RH/100)*esat)}

#calculations
satG<- e.sat(dat.allG$temp)
dat.allG$D<-vpD(satG, dat.allG$RH)

satL<- e.sat(dat.allL$temp)
dat.allL$D<-vpD(satL, dat.allL$RH)

#Kg 
kg.func<- function(Temp) {115.8 + (0.423*Temp)}
dat.allG$Kg<- kg.func(dat.allG$temp)
dat.allL$Kg<- kg.func(dat.allL$temp)

#conversion of transpiration to kg m-2 s-1 from g
El.kgG<-dat.allG[,1:16]*(1/1000)
El.kgL<-dat.allL[,1:16]*(1/1000)

#stomatal conductance (gs)
gs.func<- function (Kg.coeff, Elkg, Vpd, P)
{((Kg.coeff*Elkg)/ Vpd)*P}

Gs.rawG<-matrix(rep(NA, dim(SAG)[1]*16), ncol=16)
Gs.rawL<-matrix(rep(NA, dim(SAL)[1]*16), ncol=16)

for (i in 1:16) {
  Gs.rawG<-gs.func(dat.allG$Kg, El.kgG, dat.allG$D, dat.allG$PdayGap)
  Gs.rawL<-gs.func(dat.allL$Kg, El.kgL, dat.allL$D, dat.allL$PdayGap)
}

#conversion to mmol
mol.func<-function(Gs, temp, P) {Gs*0.446* (273/(temp+273))*(P/101.3)}
Gs.molG<- matrix(rep(NA, dim(Gs.rawG)[1]*16), ncol=16)
Gs.mmolG<-matrix(rep(NA, dim(Gs.rawG)[1]*16),ncol=16)
Gs.molL<- matrix(rep(NA, dim(Gs.rawL)[1]*16), ncol=16)
Gs.mmolL<-matrix(rep(NA, dim(Gs.rawL)[1]*16),ncol=16)
for (i in 1:16) {
  Gs.molG<-mol.func(Gs.rawG, dat.allG$temp, dat.allG$PdayGap)
  Gs.mmolG<-Gs.molG*1000
  Gs.molL<-mol.func(Gs.rawL, dat.allL$temp, dat.allL$PdayGap)
  Gs.mmolL<-Gs.molL*1000
}

#graphing stomatal conductance (mmol)


if( plotcheck ==1 ){
  
  #Graphs

for(i in 1:16){
  
  jpeg(file=paste0(plotdir, "StomatalConductance\\German\\sensor", i, ".jpeg"),
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



}

#German old code
#seperate by species: 1-8 salix, 9-16 alnus   ###NEEDS WORK!
#salix<- data.frame(gs=as.vector(Gs.mmol[,1:8]), DOY=rep(dat.all$DOY, times=8), Hour=rep(dat.all$HM, times=8))
#salix1<-aggregate(salix$gs, by=list(salix$DOY, salix$Hour), FUN="mean", na.action=na.omit)
#s.time<-salix1$Group.1+(salix1$Group.2/24)
#plot(s.time, salix1$x, pch=19)
  
  #should seperating by species not be in the process script?
  #should the 2017 field data be incorporated into this script?

