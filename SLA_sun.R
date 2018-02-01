setwd("C:\\Users\\Sabrina\\Google Drive\\Lab\\2017data\\SLA")

library(plyr)

datSLA<-read.csv("SLA_sun.csv")

datSLA$SLA<- datSLA$Area / datSLA$Mass

datSLA$Stand<- rep(seq(1,2), times=c(12,15))
datSLA$Species<- rep(c(1,2,1,2), times=c(6,6,8,7))

Sp.mean<-aggregate(datSLA$SLA, by=list(datSLA$Stand, datSLA$Species), FUN="mean")

colnames(Sp.mean)<-c("Stand", "Species", "SLA.ave")

Sp.SD<-aggregate(datSLA$SLA, by=list(datSLA$Stand, datSLA$Species), FUN="sd")
Sp.L<-aggregate(datSLA$SLA, by=list(datSLA$Stand, datSLA$Species), FUN="length")

Sp.SE<-Sp.SD$x/sqrt(Sp.L$x)

x.c<-c(1,2,4,5)
colors<-c("royalblue3", "darkseagreen", "royalblue3", "darkseagreen")

par(mai=c(1.5,1.5,1.5,1.5))
plot(c(0,1), c(0,1), type="n", xlim=c(0,6), ylim=c(100,160), axes=FALSE, xlab=" ", cex.lab=
       1.25,
     ylab= expression(paste("Specific Leaf Area (cm"^"2","g"^"-1",")")), xaxs="i", yaxs="i")

for(i in 1:dim(Sp.mean)[1]){
  polygon(c(x.c[i]-1, x.c[i]-1, x.c[i], x.c[i]),
          c(100, Sp.mean$SLA.ave[i],Sp.mean$SLA.ave[i], 100 ),col= colors[i])
}

arrows(x.c-0.5, Sp.mean$SLA.ave- Sp.SE, x.c-0.5, Sp.mean$SLA.ave + Sp.SE, code=0)

axis(2, seq(100,160,by=10), las=2)
axis(1, x.c-0.5, c("low", "high", "low", "high"))

mtext("Betula", side=1, line=3, at=1, cex=1.25)
mtext("Salix", side=1, line=3, at=4, cex=1.25)

text(1,150, "*", cex=2)

t.test(datSLA$SLA)

dat.sub<-datSLA[datSLA$Rep <=6,]

dif.betula<- dat.sub$SLA[dat.sub$Species==1& dat.sub$Stand==1]-
  dat.sub$SLA[dat.sub$Species==1& dat.sub$Stand==2]
  
t.test(dif.betula)
  
dif.betula

dif.salix<-dat.sub$SLA[dat.sub$Species==2& dat.sub$Stand==1]-
  dat.sub$SLA[dat.sub$Species==2& dat.sub$Stand==2]

t.test(dif.salix)

