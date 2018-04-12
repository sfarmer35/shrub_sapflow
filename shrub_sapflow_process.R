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
plotcheck<-0

########################################################################################
#output datG files
########################################################################################
#packages 
library(plyr)
library(lubridate)
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
Pin20G<-matrix(rep(NA,dim(SAG)[1]*16), ncol=16)
Q90G<-numeric(0)

QrL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
QfL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowUCL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowCL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
FlowLAL<-matrix(rep(NA,dim(SAL)[1]*16), ncol=16)
Q90L<-numeric(0)

#####sensor calculations######
for(i in 1:16) {
  #german
  QrG[,i]<- CG[,i]*KshAG[,i]
  QfG[,i]<- PinG[,i]-QvG[,i]-QrG[,i]
  Pin20G[,i]<- PinG[,i]*0.2
  FlowUCG[,i]<-ifelse(QfG[,i]<0,NA,
                      ifelse(dTG[,i]<0, NA, (QfG[,i])/(dTG[,i]*4.186)))
       #for sensors with many spikes setting a limit for quantile @ 0.05
  Q90G[i]<-ifelse(quantile(FlowUCG[,i], probs=0.9, na.rm=TRUE)>0.05, 0.05, 
                 quantile(FlowUCG[,i], probs=0.9, na.rm=TRUE))
       #Filter 20 percent pin
  FlowCG[,i]<-ifelse(QfG[,i]<0.20*PinG[,i]& dTG[,i] <0.75 ,0,
                      ifelse(FlowUCG[,i]> Q90G[i], NA, FlowUCG[,i]))
  FlowLAG[,i]<- FlowCG[,i]/datg.LA$LA[i]
  #lowdensity  
  QrL[,i]<- CL[,i]*KshAL[,i]
  QfL[,i]<- PinL[,i]-QvL[,i]-QrL[,i]
  FlowUCL[,i]<-ifelse(QfL[,i]<0,NA,
                      ifelse(dTL[,i]<0, NA, (QfL[,i])/(dTL[,i]*4.186)))
        #for sensors with many spikes setting a limit for quantile @ 0.05
  Q90L[i]<-ifelse(quantile(FlowUCL[,i], probs=0.9, na.rm=TRUE)>0.05, 0.05, 
                 quantile(FlowUCL[,i], probs=0.9, na.rm=TRUE))
         #Filter 20 percent pin
  FlowCL[,i]<-ifelse(QfL[,i] < 0.20 * PinL[,i] & dTL[,i] < 0.75, 0,
                    ifelse(FlowUCL[,i]> Q90L[i], NA, FlowUCL[,i]))
  FlowLAL[,i]<- FlowCL[,i]/datg.LA$LA[i]
  }

if(plotcheck==1){
  
#Graphs
for(i in 1:16){
  jpeg(file=paste0(plotdir, "\\UncorrectedFlow\\German\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowUCG)[1]), FlowUCG[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir, "\\UncorrectedFlow\\LD\\sensor", i, ".jpeg"), 
       width=1500, height=1000, units="px")
  plot(seq(1:dim(FlowUCL)[1]), FlowUCL[,i], xlab="time", ylab="Flow ", type="b",
       main=paste("sensor #", i), pch=19)
  dev.off()
}  
  
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
datesL<- data.frame(doy=datetableL$doy, hour=datetableL$hour)
datesL$hourO<- floor(datesL$hour)
datesL$min<- datesL$hour- datesL$hourO
datesL$minf1<- ifelse(datesL$min>0.7, 1, datesL$min)
datesL$minf2<- ifelse(datesL$minf1>0.2&datesL$minf1<0.3, 0.50, datesL$minf1)
datesL$hourfix<- datesL$hourO + datesL$minf2

#joins
datg.Met$DHM<-datg.Met$DOY+(datg.Met$HM/24)
E.allG<-data.frame(FlowLAG[,1:16], DOY=datetableG$doy, HM=datetableG$hour)
dat.mostG<- join(E.allG, datg.Met, by=c("DOY", "HM"), type="left")
dat.allG<- join(dat.mostG, datP, by=c("DOY"), type="left")
  
El.allL<- data.frame(FlowLAL[,1:16], doy= datesL$doy, hour=datesL$hourfix)
dat.mostL<- join(El.allL, datld.Met, by=c("doy","hour"), type="left")
datPL<-datP
colnames(datPL)<- c("doy", "year", "Pkpa", "PdayGap")
dat.allL<- join(dat.mostL, datPL, by=c("doy"), type= "left")

#vapor pressure defecit
#making functions
e.sat<- function(Temp) { 0.611*exp((17.502*Temp)/(Temp+240.97)) }
vpD<- function(esat,RH) {esat-((RH/100)*esat)}

#calculations
satG<- e.sat(dat.allG$temp)
dat.allG$D<-vpD(satG, dat.allG$RH)

satL<- e.sat(dat.allL$temp)
dat.allL$D<-vpD(satL, dat.allL$RH)

### HYSTERISIS CHECK ####

if (plotcheck ==1) {
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\t0\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allG$D, dat.allG[,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\LD\\t0\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allL$D, dat.allL[,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

#day 187
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\day187\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allG$D[dat.allG$DOY==187], dat.allG[dat.allG$DOY==187,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\LD\\day187\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allL$D[dat.allL$doy==187], dat.allL[dat.allL$doy==187,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

#day 197
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\day197\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allG$D[dat.allG$DOY==197], dat.allG[dat.allG$DOY==197,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\LD\\day197\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allL$D[dat.allL$doy==197], dat.allL[dat.allL$doy==197,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}

#day 207
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\day207\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allG$D[dat.allG$DOY==207], dat.allG[dat.allG$DOY==207,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
for(i in 1:16){
  jpeg(file=paste0(plotdir,"\\Hysteresis\\LD\\day207\\sensor", i, ".jpeg"), width=1500,
       height=1000, units="px")
  plot(dat.allL$D[dat.allL$doy==207], dat.allL[dat.allL$doy==207,i],  xlab="vpd", ylab="flow ", type="p",
       main=paste("sensor #", i), pch=19)
  dev.off()
}
}

datHG<-dat.allG
datHG$hour.m30<- datHG$HM- 0.5
datHG$hour.m60<- datHG$HM - 1
datHG$hour.m90<- datHG$HM-1.5
datHG$hour.m120<- datHG$HM-2
vpdG1<- data.frame(hour.m30=dat.allG$HM, vpd.30= dat.allG$D, DOY= dat.allG$DOY)
vpdG2<- data.frame(hour.m60=dat.allG$HM, vpd.60= dat.allG$D, DOY= dat.allG$DOY)
vpdG3<- data.frame(hour.m90=dat.allG$HM, vpd.90= dat.allG$D, DOY= dat.allG$DOY)
vpdG4<- data.frame(hour.m120=dat.allG$HM, vpd.120= dat.allG$D, DOY= dat.allG$DOY)

datHG<- join(datHG, vpdG1, by= c("hour.m30", "DOY"), type= "left")
datHG<- join(datHG, vpdG2, by= c("hour.m60", "DOY"), type= "left")
datHG<- join(datHG, vpdG3, by= c("hour.m90", "DOY"), type= "left")
datHG<- join(datHG, vpdG4, by= c("hour.m120", "DOY"), type= "left")

#individual days graph

#seperate by species: 1-8 salix, 9-16 alnus 
#german alnus
datHG.a<-data.frame(DOY= rep(datHG$DOY, times=8),HM= rep(datHG$HM, times=8), 
                    vpd= rep(datHG$D, times= 8), vpd.30= rep(datHG$vpd.30, times=8), 
                    vpd.60= rep(datHG$vpd.60, times=8), vpd.90= rep(datHG$vpd.90, times=8),
                    vpd.120= rep(datHG$vpd.120, times=8), flow=as.vector(data.matrix(datHG[,9:16])))
datHG.a$flowc<-ifelse(datHG.a$flow==0, NA, datHG.a$flow)

plot(datHG.a$vpd[datHG.a$vpd>= 0.6], datHG.a$flowc[datHG.a$vpd>= 0.6])
HG.a0<- lm(datHG.a$flowc[datHG.a$vpd>= 0.6]~datHG.a$vpd[datHG.a$vpd>= 0.6])
summary(HG.a0)

plot(datHG.a$vpd.30[datHG.a$vpd>= 0.6],datHG.a$flowc[datHG.a$vpd>= 0.6])
HG.a30<- lm(datHG.a$flowc[datHG.a$vpd.30>= 0.6]~datHG.a$vpd.30[datHG.a$vpd.30>= 0.6])
summary(HG.a30)

plot(datHG.a$vpd.60[datHG.a$vpd>= 0.6],datHG.a$flowc[datHG.a$vpd>= 0.6])
HG.a60<- lm(datHG.a$flowc[datHG.a$vpd.60>= 0.6]~datHG.a$vpd.60[datHG.a$vpd.60>= 0.6])
summary(HG.a60)

#day 187
plot(datHG.a$vpd[datHG.a$vpd>= 0.6 & datHG.a$DOY==187], datHG.a$flowc[datHG.a$vpd>=
                                                                        0.6 & datHG.a$DOY==187])
HG.a0<- lm(datHG.a$flowc[datHG.a$vpd>= 0.6& datHG.a$DOY==187]~
             datHG.a$vpd[datHG.a$vpd>= 0.6 & datHG.a$DOY==187])
summary(HG.a0)

plot(datHG.a$vpd.30[datHG.a$vpd.30>= 0.6 & datHG.a$DOY==187], datHG.a$flowc[datHG.a$vpd.30>=
                                                                              0.6 & datHG.a$DOY==187])
HG.a30<- lm(datHG.a$flowc[datHG.a$vpd.30>= 0.6& datHG.a$DOY==187]~
              datHG.a$vpd.30[datHG.a$vpd.30>= 0.6 & datHG.a$DOY==187])
summary(HG.a30)

plot(datHG.a$vpd.60[datHG.a$vpd.60>= 0.6 & datHG.a$DOY==187], datHG.a$flowc[datHG.a$vpd.60>=
                                                                              0.6 & datHG.a$DOY==187])
HG.a60<- lm(datHG.a$flowc[datHG.a$vpd.60>= 0.6& datHG.a$DOY==187]~
              datHG.a$vpd.60[datHG.a$vpd.60>= 0.6 & datHG.a$DOY==187])
summary(HG.a60)

#german salix
datHG.s<-data.frame(DOY= rep(datHG$DOY, times=8), HM= rep(datHG$HM, times=8),
    vpd= rep(datHG$D, times= 8),vpd.30= rep(datHG$vpd.30, times=8),  vpd.60= rep(datHG$vpd.60,times=8),
    vpd.90= rep(datHG$vpd.90, times=8), vpd.120= rep(datHG$vpd.120, times=8),
    flow=as.vector(data.matrix(datHG[,1:8])))
datHG.s$flowc<-ifelse(datHG.s$flow==0, NA, datHG.s$flow)

plot(datHG.s$vpd[datHG.s$vpd >= 0.6], datHG.s$flowc[datHG.s$vpd>= 0.6])
HG.s0<- lm(datHG.s$flowc[datHG.s$vpd>= 0.6]~datHG.s$vpd[datHG.s$vpd>= 0.6])
summary(HG.s0)

plot(datHG.s$vpd.30 [datHG.s$vpd.30 >= 0.6],datHG.s$flowc[datHG.s$vpd.30 >= 0.6])
HG.s30<- lm(datHG.s$flowc[datHG.s$vpd.30>= 0.6]~datHG.s$vpd.30[datHG.s$vpd.30>= 0.6])
summary(HG.s30)

plot(datHG.s$vpd.60 [datHG.s$vpd.60 >= 0.6],datHG.s$flowc [datHG.s$vpd.60 >= 0.6])
HG.s60<- lm(datHG.s$flowc[datHG.s$vpd.60>= 0.6]~datHG.s$vpd.60[datHG.s$vpd.60>= 0.6])
summary(HG.s60)

dev.off()


#ld species?
#1,2,3,4,7,9,10,11,12,14,16 
#5,6,8,13,15

#German Alnus Hysteresis Check
alderHH<-aggregate(datHG.a$flowc, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
                     "na.omit", na.rm= TRUE)
alderV<-aggregate(datHG.a$vpd, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
                     "na.omit", na.rm= TRUE)
alderV30<-aggregate(datHG.a$vpd.30, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
          "na.omit", na.rm= TRUE)
alderV60<-aggregate(datHG.a$vpd.60, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
          "na.omit", na.rm= TRUE)
alderV90<-aggregate(datHG.a$vpd.90, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
                      "na.omit", na.rm= TRUE)
alderV120<-aggregate(datHG.a$vpd.120, by= list(datHG.a$HM, datHG.a$DOY), FUN = "mean", na.action=
                      "na.omit", na.rm= TRUE)

colnames(alderHH)<- c("HM", "DOY", "flowc")
colnames(alderV)<- c("HM", "DOY", "VPD")
colnames(alderV30)<- c("HM", "DOY", "VPD")
colnames(alderV60)<- c("HM", "DOY", "VPD")
colnames(alderV90)<- c("HM", "DOY", "VPD")
colnames(alderV120)<- c("HM", "DOY", "VPD")

alderV$flowc<- alderHH$flowc
alderV30$flowc<-alderHH$flowc
alderV60$flowc<-alderHH$flowc
alderV90$flowc<-alderHH$flowc
alderV120$flowc<-alderHH$flowc

plot(alderHH$DOY+(alderHH$HM/24), alderHH$flowc)

alderday<-data.frame(DOY= unique(alderHH$DOY))

#vpd 0
if (plotcheck==1){
coli<-rainbow(10)
jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup1", ".jpeg"), width=1500,
     height=1000, units="px")
plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.05), xlab="VPD", ylab= "Flow")
for(i in 1:10){
  points(alderV$VPD[alderV$DOY==alderday$DOY[i]], alderHH$flowc[alderV$DOY==alderday$DOY[i]],
         pch= 19, type= "b", col= coli[i], cex=2)
  text(alderV$VPD[alderV$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV$DOY==alderday$DOY[i]]
       +0.001, paste(as.character(alderV$HM[alderV$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
}
dev.off()

coli<-rainbow(10)
jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup2", ".jpeg"), width=1500,
     height=1000, units="px")
plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
for(i in 11:20){
  points(alderV$VPD[alderV$DOY==alderday$DOY[i]], alderHH$flowc[alderV$DOY==alderday$DOY[i]],
         pch= 19, type= "b", col= coli[i-10], cex=2)
  text(alderV$VPD[alderV$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV$DOY==alderday$DOY[i]]
       +0.001, paste(as.character(alderV$HM[alderV$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
}
dev.off()

coli<-rainbow(10)
jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup3", ".jpeg"), width=1500,
     height=1000, units="px")
plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
for(i in 21:30){
  points(alderV$VPD[alderV$DOY==alderday$DOY[i]], alderHH$flowc[alderV$DOY==alderday$DOY[i]],
         pch= 19, type= "b", col= coli[i-20], cex=2)
  text(alderV$VPD[alderV$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV$DOY==alderday$DOY[i]]
       +0.001, paste(as.character(alderV$HM[alderV$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
}
dev.off()

coli<-rainbow(10)
jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup4", ".jpeg"), width=1500,
     height=1000, units="px")
plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
for(i in 31:40){
  points(alderV$VPD[alderV$DOY==alderday$DOY[i]], alderHH$flowc[alderV$DOY==alderday$DOY[i]],
         pch= 19, type= "b", col= coli[i-30], cex=2)
  text(alderV$VPD[alderV$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV$DOY==alderday$DOY[i]]
       +0.001, paste(as.character(alderV$HM[alderV$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
}
dev.off()

coli<-rainbow(10)
jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup5", ".jpeg"), width=1500,
     height=1000, units="px")
plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
for(i in 41:50){
  points(alderV$VPD[alderV$DOY==alderday$DOY[i]], alderHH$flowc[alderV$DOY==alderday$DOY[i]],
         pch= 19, type= "b", col= coli[i-40], cex=2)
  text(alderV$VPD[alderV$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV$DOY==alderday$DOY[i]]
       +0.001, paste(as.character(alderV$HM[alderV$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
}
dev.off()
  }
#vpd 30
if (plotcheck==1){
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup1_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.05), xlab="VPD", ylab= "Flow")
  for(i in 1:10){
    points(alderV30$VPD[alderV30$DOY==alderday$DOY[i]], alderHH$flowc[alderV30$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i], cex=2)
    text(alderV30$VPD[alderV30$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV30$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV30$HM[alderV30$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup2_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 11:20){
    points(alderV30$VPD[alderV30$DOY==alderday$DOY[i]], alderHH$flowc[alderV30$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-10], cex=2)
    text(alderV30$VPD[alderV30$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV30$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV30$HM[alderV30$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup3_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 21:30){
    points(alderV30$VPD[alderV30$DOY==alderday$DOY[i]], alderHH$flowc[alderV30$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-20], cex=2)
    text(alderV30$VPD[alderV30$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV30$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV30$HM[alderV30$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup4_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 31:40){
    points(alderV30$VPD[alderV30$DOY==alderday$DOY[i]], alderHH$flowc[alderV30$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-30], cex=2)
    text(alderV30$VPD[alderV30$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV30$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV30$HM[alderV30$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup5_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 41:50){
    points(alderV30$VPD[alderV30$DOY==alderday$DOY[i]], alderHH$flowc[alderV30$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-40], cex=2)
    text(alderV30$VPD[alderV30$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV30$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV30$HM[alderV30$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
}
#vpd 60
if (plotcheck==1){
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup1_60", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.05), xlab="VPD", ylab= "Flow")
  for(i in 1:10){
    points(alderV60$VPD[alderV60$DOY==alderday$DOY[i]], alderHH$flowc[alderV60$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i], cex=2)
    text(alderV60$VPD[alderV60$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV60$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV60$HM[alderV60$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup2_60", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 11:20){
    points(alderV60$VPD[alderV60$DOY==alderday$DOY[i]], alderHH$flowc[alderV60$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-10], cex=2)
    text(alderV60$VPD[alderV60$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV60$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV60$HM[alderV60$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup3_60", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 21:30){
    points(alderV60$VPD[alderV60$DOY==alderday$DOY[i]], alderHH$flowc[alderV60$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-20], cex=2)
    text(alderV60$VPD[alderV60$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV60$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV60$HM[alderV60$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup4_60", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 31:40){
    points(alderV60$VPD[alderV60$DOY==alderday$DOY[i]], alderHH$flowc[alderV60$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-30], cex=2)
    text(alderV60$VPD[alderV60$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV60$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV60$HM[alderV60$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\AlnusDayGroup5_60", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 41:50){
    points(alderV60$VPD[alderV60$DOY==alderday$DOY[i]], alderHH$flowc[alderV60$DOY==alderday$DOY[i]],
           pch= 19, type= "b", col= coli[i-40], cex=2)
    text(alderV60$VPD[alderV60$DOY==alderday$DOY[i]]+0.01, alderHH$flowc[alderV60$DOY==alderday$DOY[i]]
         +0.001, paste(as.character(alderV60$HM[alderV60$DOY==alderday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
}
#add in these plots for 90 and 120?

#German Salix hysteresis
salixHH<-aggregate(datHG.s$flowc, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                     "na.omit", na.rm= TRUE)
salixV<-aggregate(datHG.s$vpd, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                    "na.omit", na.rm= TRUE)
salixV30<-aggregate(datHG.s$vpd.30, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                     "na.omit", na.rm= TRUE)
salixV60<-aggregate(datHG.s$vpd.60, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                      "na.omit", na.rm= TRUE)
salixV90<-aggregate(datHG.s$vpd.90, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                      "na.omit", na.rm= TRUE)
salixV120<-aggregate(datHG.s$vpd.120, by= list(datHG.s$HM, datHG.s$DOY), FUN = "mean", na.action=
                      "na.omit", na.rm= TRUE)

colnames(salixHH)<- c("HM", "DOY", "flowc")
colnames(salixV)<- c("HM", "DOY", "VPD")
colnames(salixV30)<- c("HM", "DOY", "VPD")
colnames(salixV60)<- c("HM", "DOY", "VPD")
colnames(salixV90)<- c("HM", "DOY", "VPD")
colnames(salixV120)<- c("HM", "DOY", "VPD")
salixday<-data.frame(DOY= unique(salixHH$DOY))

salixV$flowc<- salixHH$flowc
salixV30$flowc<-salixHH$flowc
salixV60$flowc<-salixHH$flowc
salixV90$flowc<-salixHH$flowc
salixV120$flowc<-salixHH$flowc


#vpd 0 german salix
if (plotcheck==1){
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup1", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.08), xlab="VPD", ylab= "Flow")
  for(i in 1:10){
    points(salixV$VPD[salixV$DOY==salixday$DOY[i]], salixHH$flowc[salixV$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i], cex=2)
    text(salixV$VPD[salixV$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV$HM[salixV$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup2", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 11:20){
    points(salixV$VPD[salixV$DOY==salixday$DOY[i]], salixHH$flowc[salixV$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-10], cex=2)
    text(salixV$VPD[salixV$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV$HM[salixV$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup3", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 21:30){
    points(salixV$VPD[salixV$DOY==salixday$DOY[i]], salixHH$flowc[salixV$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-20], cex=2)
    text(salixV$VPD[salixV$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV$HM[salixV$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup4", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 31:40){
    points(salixV$VPD[salixV$DOY==salixday$DOY[i]], salixHH$flowc[salixV$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-30], cex=2)
    text(salixV$VPD[salixV$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV$HM[salixV$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup5", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 41:50){
    points(salixV$VPD[salixV$DOY==salixday$DOY[i]], salixHH$flowc[salixV$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-40], cex=2)
    text(salixV$VPD[salixV$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV$HM[salixV$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
}

#vpd 30 german salix
if (plotcheck==1){
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup1_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.08), xlab="VPD", ylab= "Flow")
  for(i in 1:10){
    points(salixV30$VPD[salixV30$DOY==salixday$DOY[i]], salixHH$flowc[salixV30$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i], cex=2)
    text(salixV30$VPD[salixV30$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV30$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV30$HM[salixV30$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup2_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 11:20){
    points(salixV30$VPD[salixV30$DOY==salixday$DOY[i]], salixHH$flowc[salixV30$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-10], cex=2)
    text(salixV30$VPD[salixV30$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV30$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV30$HM[salixV30$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup3_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 21:30){
    points(salixV30$VPD[salixV30$DOY==salixday$DOY[i]], salixHH$flowc[salixV30$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-20], cex=2)
    text(salixV30$VPD[salixV30$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV30$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV30$HM[salixV30$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup4_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 31:40){
    points(salixV30$VPD[salixV30$DOY==salixday$DOY[i]], salixHH$flowc[salixV30$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-30], cex=2)
    text(salixV30$VPD[salixV30$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV30$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV30$HM[salixV30$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
  
  coli<-rainbow(10)
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\SalixDayGroup5_30", ".jpeg"), width=1500,
       height=1000, units="px")
  plot(c(0,1), c(0,1), xlim=c(0,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  for(i in 41:50){
    points(salixV30$VPD[salixV30$DOY==salixday$DOY[i]], salixHH$flowc[salixV30$DOY==salixday$DOY[i]],
           pch= 19, type= "b", col= coli[i-40], cex=2)
    text(salixV30$VPD[salixV30$DOY==salixday$DOY[i]]+0.01, salixHH$flowc[salixV30$DOY==salixday$DOY[i]]
         +0.001, paste(as.character(salixV30$HM[salixV30$DOY==salixday$DOY[i]])), cex=2, col= coli[i])
  }
  dev.off()
}
}
#hysteresis check 2

#fix order? (put alnus first compared to salix)
###HELP###
#confused what this code is about....
salixVs<- salixV[salixV$VPD>= 0.6  & salixV30$HM>= 10& salixV30$HM<= 19, ]
salixV30s<- salixV30[salixV30$VPD>= 0.6& salixV30$HM>= 10& salixV30$HM<= 19, ]
salixV60s<- salixV60[salixV60$VPD>= 0.6& salixV30$HM>= 10& salixV30$HM<= 19, ]

#think I may have just messed it up from when I moved it from here. Is this correct???:
alderVs<- alderV[alderV$VPD>=0.6 &alderV$HM>= 10 & alderV$HM<=19,]
alderV30s<- alderV30[alderV30$VPD>=0.6 &alderV30$HM>= 10 & alderV30$HM<=19,]
alderV60s<- alderV60[alderV60$VPD>=0.6&alderV60$HM>= 10 & alderV60$HM<=19,]
alderV90s<- alderV90[alderV90$VPD>=0.6&alderV90$HM>= 10 & alderV90$HM<=19,]
alderV120s<- alderV120[alderV120$VPD>=0.6&alderV120$HM>= 10 & alderV120$HM<=19,]

nsalixV<-aggregate(salixVs$DOY, by=list(salixVs$DOY), FUN = "length")
nsalixV30<-aggregate(salixV30s$DOY, by=list(salixV30s$DOY), FUN = "length")
nsalixV60<-aggregate(salixV60s$DOY, by=list(salixV60s$DOY), FUN = "length")
nsalixV90<-aggregate(salixV90s$DOY, by=list(salixV90s$DOY), FUN = "length")
nsalixV120<-aggregate(salixV120s$DOY, by=list(salixV120s$DOY), FUN = "length")

#fix salix stuff when all is right 
colnames(nsalixV)<- c("DOY", "n")
colnames(nsalixV30)<- c("DOY", "n")
colnames(nsalixV60)<- c("DOY", "n")

nalderV<-aggregate(alderVs$DOY, by= list(alderVs$DOY), FUN = "length")
nalderV30<-aggregate(alderV30s$DOY, by= list(alderV30s$DOY), FUN = "length")
nalderV60<-aggregate(alderV60s$DOY, by= list(alderV60s$DOY), FUN = "length")
nalderV90<-aggregate(alderV90s$DOY, by= list(alderV90s$DOY), FUN = "length")
nalderV120<-aggregate(alderV120s$DOY, by= list(alderV120s$DOY), FUN = "length")
colnames(nalderV)<- c("DOY", "n")
colnames(nalderV30)<- c("DOY", "n")
colnames(nalderV60)<- c("DOY", "n")
colnames(nalderV90)<- c("DOY", "n")
colnames(nalderV120)<- c("DOY", "n")

nsalixV<- nsalixV[nsalixV$n>3,]
nsalixV30<- nsalixV30[nsalixV30$n>3,]
nsalixV60<- nsalixV60[nsalixV60$n>3,]

nalderV<- nalderV[nalderV$n>3,]
nalderV30<- nalderV30[nalderV30$n>3,]
nalderV60<- nalderV60[nalderV60$n>3,]
nalderV90<- nalderV90[nalderV90$n>3,]
nalderV120<- nalderV120[nalderV120$n>3,]

#regression of agg values
aL0<- lm(alderVs$flowc~log(alderVs$VPD))
summary(aL0)
plot(log(alderVs$VPD), alderVs$flowc)
aL30<- lm(alderV30s$flowc~log(alderV30s$VPD))
summary(aL30)
plot(log(alderV30s$VPD), alderV30s$flowc)

s.rs.df<- data.frame(DOY=nalderV$DOY)
s.rs.df30<-data.frame(DOY=nalderV$DOY)
s.rs.df60<-data.frame(DOY=nalderV$DOY)

a.rs.df<- data.frame(DOY=nalderV$DOY)
a.rs.df30<-data.frame(DOY=nalderV$DOY)
a.rs.df60<-data.frame(DOY=nalderV$DOY)
a.rs.df90<-data.frame(DOY=nalderV$DOY)
a.rs.df120<-data.frame(DOY=nalderV$DOY)


#alder day figs
if (plotcheck== 1){
lmalder<- list()
#logvpd
for (i in 1:dim(nalderV)[1]){
  lmalder[[i]]<- lm(alderVs$flowc[alderVs$DOY==nalderV$DOY[i]]~log(alderVs$VPD[alderVs$DOY==nalderV$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\zero\\doy", nalderV$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6, 1), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(alderVs$VPD[alderVs$DOY==nalderV$DOY[i]]), alderVs$flowc[alderVs$DOY==nalderV$DOY[i]],
         pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmalder[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmalder[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmalder[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmalder[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmalder[[i]])
  a.rs.df$r.sq[i]<- summary(lmalder[[i]])$r.squared
  a.rs.df$slope[i]<-summary(lmalder[[i]])$coefficients[2,1]
  a.rs.df$pvalue[i]<-summary(lmalder[[i]])$coefficients[2,4]
  dev.off()
  
}
#30
lmalder30<- list()
for (i in 1:dim(nalderV30)[1]){
  lmalder30[[i]]<- lm(alderV30s$flowc[alderV30s$DOY==nalderV30$DOY[i]]~log(alderV30s$VPD[alderV30s$DOY==
  nalderV30$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\thirty\\doy", nalderV30$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,1), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(alderV30s$VPD[alderV30s$DOY==nalderV30$DOY[i]]), alderV30s$flowc[alderV30s$DOY==
  nalderV30$DOY[i]], pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmalder30[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmalder30[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmalder30[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmalder30[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmalder30[[i]])
  a.rs.df30$r.sq30[i]<- summary(lmalder30[[i]])$r.squared
  a.rs.df30$slope30[i]<-summary(lmalder30[[i]])$coefficients[2,1]
  a.rs.df30$pvalue30[i]<-summary(lmalder30[[i]])$coefficients[2,4]
  dev.off()
  
}
#60
lmalder60<- list()
for (i in 1:dim(nalderV60)[1]){
  lmalder60[[i]]<- lm(alderV60s$flowc[alderV60s$DOY==nalderV60$DOY[i]]~log(alderV60s$VPD[alderV60s$DOY==
    nalderV60$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\sixty\\doy", nalderV60$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,1), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(alderV60s$VPD[alderV60s$DOY==nalderV60$DOY[i]]), alderV60s$flowc[alderV60s$DOY==
    nalderV60$DOY[i]], pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmalder60[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmalder60[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmalder60[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmalder60[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmalder60[[i]])
  a.rs.df60$r.sq60[i]<- summary(lmalder60[[i]])$r.squared
  a.rs.df60$slope60[i]<-summary(lmalder60[[i]])$coefficients[2,1]
  a.rs.df60$pvalue60[i]<-summary(lmalder60[[i]])$coefficients[2,4]
  dev.off()
  
}
#90
lmalder90<- list()
for (i in 1:dim(nalderV90)[1]){
  lmalder90[[i]]<- lm(alderV90s$flowc[alderV90s$DOY==nalderV90$DOY[i]]~log(alderV90s$VPD[alderV90s$DOY==
                                                                                           nalderV90$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\sixty\\doy", nalderV90$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,1), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(alderV90s$VPD[alderV90s$DOY==nalderV90$DOY[i]]), alderV90s$flowc[alderV90s$DOY==
                                                                                nalderV90$DOY[i]], pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmalder90[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmalder90[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmalder90[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmalder90[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmalder90[[i]])
  a.rs.df90$r.sq90[i]<- summary(lmalder90[[i]])$r.squared
  a.rs.df90$slope90[i]<-summary(lmalder90[[i]])$coefficients[2,1]
  a.rs.df90$pvalue90[i]<-summary(lmalder90[[i]])$coefficients[2,4]
  dev.off()
}
  
#120
  lmalder120<- list()
  for (i in 1:dim(nalderV120)[1]){
    lmalder120[[i]]<- lm(alderV120s$flowc[alderV120s$DOY==nalderV120$DOY[i]]~log(alderV120s$VPD[alderV120s$DOY==
                                                                                             nalderV120$DOY[i]]))
    jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\sixty\\doy", nalderV120$DOY[i], ".jpeg"),
         width=1500,height=1000, units="px")
    par(mai=c(2,2,2,2))
    plot(c(0,1), c(0,1), xlim=c(-0.6,1), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
    points(log(alderV120s$VPD[alderV120s$DOY==nalderV120$DOY[i]]), alderV120s$flowc[alderV120s$DOY==
                                                                                  nalderV120$DOY[i]], pch= 19, type= "p", cex=2)
    mtext(paste("y=", round(summary(lmalder120[[i]])$coefficients[1,1],3 ), "+", 
                round(summary(lmalder120[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
    mtext(paste("R2=", round(summary(lmalder120[[i]])$r.squared, 3)), side=3, line=4, cex=2)
    mtext(paste("p=", round(summary(lmalder120[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
    abline(lmalder120[[i]])
    a.rs.df120$r.sq120[i]<- summary(lmalder120[[i]])$r.squared
    a.rs.df120$slope120[i]<-summary(lmalder120[[i]])$coefficients[2,1]
    a.rs.df120$pvalue120[i]<-summary(lmalder120[[i]])$coefficients[2,4]
    dev.off()
  
}
}

#salix day figs 
if (plotcheck==1){

  lmsalix<- list()
  for (i in 1:dim(nsalixV)[1]){
    lmsalix[[i]]<- lm(salixVs$flowc[salixVs$DOY==nsalixV$DOY[i]]~log(salixVs$VPD[salixVs$DOY==nsalixV$DOY[i]]))
    jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\zero\\doyS", nsalixV$DOY[i], ".jpeg"),
         width=1500,height=1000, units="px")
    par(mai=c(2,2,2,2))
    plot(c(0,1), c(0,1), xlim=c(-0.6,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
    points(log(salixVs$VPD[salixVs$DOY==nsalixV$DOY[i]]), salixVs$flowc[salixVs$DOY==nsalixV$DOY[i]],
           pch= 19, type= "p", cex=2)
    mtext(paste("y=", round(summary(lmsalix[[i]])$coefficients[1,1],3 ), "+", 
                round(summary(lmsalix[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
    mtext(paste("R2=", round(summary(lmsalix[[i]])$r.squared, 3)), side=3, line=4, cex=2)
    mtext(paste("p=", round(summary(lmsalix[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
    abline(lmsalix[[i]])
    s.rs.df$r.sq[i]<- summary(lmsalix[[i]])$r.squared
    s.rs.df$slope[i]<-summary(lmsalix[[i]])$coefficients[2,1]
    s.rs.df$pvalue[i]<-summary(lmsalix[[i]])$coefficients[2,4]
    dev.off()
}
lmsalix30<- list()
for (i in 1:dim(nsalixV30)[1]){
  lmsalix30[[i]]<- lm(salixV30s$flowc[salixV30s$DOY==nsalixV30$DOY[i]]~log(salixV30s$VPD[salixV30s$DOY==nsalixV30$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\zero\\doyS", nsalixV30$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(salixV30s$VPD[salixV30s$DOY==nsalixV30$DOY[i]]), salixV30s$flowc[salixV30s$DOY==nsalixV30$DOY[i]],
         pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmsalix30[[i]])$coefficients[1,1],3 ), "+", 
             round(summary(lmsalix30[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  #mtext(paste("R2=", round(summary(lmsalix30[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  #mtext(paste("p=", round(summary(lmsalix30[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  #abline(lmsalix30[[i]])
  #s.rs.df30$r.sq30[i]<- summary(lmsalix30[[i]])$r.squared
  #s.rs.df30$slope30[i]<-summary(lmsalix30[[i]])$coefficients[2,1]
  #s.rs.df30$pvalue30[i]<-summary(lmsalix30[[i]])$coefficients[2,4]
  dev.off()
}

lmsalix90<- list()
for (i in 1:dim(nsalixV90)[1]){
  lmsalix90[[i]]<- lm(salixV90s$flowc[salixV90s$DOY==nsalixV90$DOY[i]]~log(salixV90s$VPD[salixV90s$DOY==nsalixV90$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\zero\\doyS", nsalixV90$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(salixV90s$VPD[salixV90s$DOY==nsalixV90$DOY[i]]), salixV90s$flowc[salixV90s$DOY==nsalixV90$DOY[i]],
         pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmsalix90[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmsalix90[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmsalix90[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmsalix90[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmsalix90[[i]])
  s.rs.df90$r.sq90[i]<- summary(lmsalix90[[i]])$r.squared
  s.rs.df90$slope90[i]<-summary(lmsalix90[[i]])$coefficients[2,1]
  s.rs.df90$pvalue90[i]<-summary(lmsalix90[[i]])$coefficients[2,4]
  dev.off()
}
lmsalix120<- list()
for (i in 1:dim(nsalixV120)[1]){
  lmsalix120[[i]]<- lm(salixV120s$flowc[salixV120s$DOY==nsalixV120$DOY[i]]~log(salixV120s$VPD[salixV120s$DOY==nsalixV120$DOY[i]]))
  jpeg(file=paste0(plotdir,"\\Hysteresis\\German\\Regression\\zero\\doyS", nsalixV120$DOY[i], ".jpeg"),
       width=1500,height=1000, units="px")
  par(mai=c(2,2,2,2))
  plot(c(0,1), c(0,1), xlim=c(-0.6,2.5), ylim= c(0, 0.1), xlab="VPD", ylab= "Flow")
  points(log(salixV120s$VPD[salixV120s$DOY==nsalixV120$DOY[i]]), salixV120s$flowc[salixV120s$DOY==nsalixV120$DOY[i]],
         pch= 19, type= "p", cex=2)
  mtext(paste("y=", round(summary(lmsalix120[[i]])$coefficients[1,1],3 ), "+", 
              round(summary(lmsalix120[[i]])$coefficients[2,1],3 ), "xVPD"), side=3, line=2,cex=2)
  mtext(paste("R2=", round(summary(lmsalix120[[i]])$r.squared, 3)), side=3, line=4, cex=2)
  mtext(paste("p=", round(summary(lmsalix120[[i]])$coefficients[2,4],3 )), side=3, line=6, cex=2)
  abline(lmsalix120[[i]])
  s.rs.df120$r.sq120[i]<- summary(lmsalix120[[i]])$r.squared
  s.rs.df120$slope120[i]<-summary(lmsalix120[[i]])$coefficients[2,1]
  s.rs.df120$pvalue120[i]<-summary(lmsalix120[[i]])$coefficients[2,4]
  dev.off()
}
}


a.rs.df.some<- join(a.rs.df,a.rs.df30, by= "DOY")
a.rs.df.some1<- join(a.rs.df.some, a.rs.df60, by = "DOY")
a.rs.df.some2<- join(a.rs.df.some1, a.rs.df90, by = "DOY")
a.rs.df.all<- join(a.rs.df.some2, a.rs.df120, by= "DOY")

r.sq.all<-data.frame(r.0= a.rs.df.all$r.sq, r.30= a.rs.df.all$r.sq30, r.60= a.rs.df.all$r.sq60,
                     r.90= a.rs.df.all$r.sq90, r.120= a.rs.df.all$r.sq120)

plot(~r.sq.all$r.0+r.sq.all$r.30+r.sq.all$r.60)
a.rs.df.all$rdiff1<- a.rs.df.all$r.sq-a.rs.df.all$r.sq30
a.rs.df.all$rdiff2<- a.rs.df.all$r.sq-a.rs.df.all$r.sq60
a.rs.df.all$rdiff3<- a.rs.df.all$r.sq30-a.rs.df.all$r.sq60

par(mfrow=c(1,5))
hist(a.rs.df.all$r.sq, breaks= seq(0,1, by=0.1))
hist(a.rs.df.all$r.sq30, breaks= seq(0,1, by=0.1))
hist(a.rs.df.all$r.sq60, breaks= seq(0,1, by=0.1))
hist(a.rs.df.all$r.sq90, breaks= seq(0,1, by=0.1))
hist(a.rs.df.all$r.sq120, breaks= seq(0,1, by=0.1))
alder.rsq<-data.frame(colMeans(r.sq.all))
  #gets better with time, what about 90 and 120  back?

s.rs.df.some<- join(s.rs.df, a.rs.df30, by = "DOY")
s.rs.df.all<- join(a.rs.df.som, a.rs.df60, by= "DOY")

