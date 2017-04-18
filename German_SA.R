# set wd
setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\sapflow_sf\\German")

#input variables data
datSA<-read.csv("Area_inputvalues.csv")

C<-datSA[5:20]
B<-datSA[21:36]
A<-datSA[37:52]
Pin<-datSA[53:68]
dT<-datSA[69:84]
SA<-datSA[85:100]
  #these values are now their own "data". What does it mean to be "data" vs "values"?
  #which do I want them to be?

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