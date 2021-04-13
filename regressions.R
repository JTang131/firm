library(lfe)
library(sandwich)
library(lmtest)
library(plm)
library(ggplot2)
library(pracma)
library(stargazer)
library(fixest)
library(data.table)
library(lme4)
library(dplyr)
library(latex2exp)


#LP Horizon 1
dat1 <- read.csv("E:/firm project/data/dat1.csv", header=FALSE)

attach(dat1)
#dat1=dat1[dat1$V4<2008]
indyear=dat1$V3*10000+dat1$V4
dat1$indyear=indyear
reg1=felm(V1~V5+I(V5*V6)+V7+V8|indyear+V2|0|V2,data = dat1)
summary(reg1)
r10=felm(V1~V5+I(V5*V9)+V7+V8|indyear+V2|0|V2,data = dat1)
summary(r10)
r1=feols(V1~V5+I(V5*V6)|indyear+V2,data=dat1)

dat2 <- read.csv("E:/firm project/data/dat2.csv", header=FALSE)
attach(dat2)
dat2=dat2[dat2$V4<2011,]
#lp2r=lp2r[lp2r$V10<1981,]
indyear=dat2$V3*10000+dat2$V4
reg21=felm(V1~V5+V6+V8+V12+V13|indyear+V2|0|V2,data = dat2)
summary(reg21)
reg22=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = dat2)
summary(reg22)
reg23=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = dat2)
summary(reg23)
reg24=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = dat2)
summary(reg24)
wage=dat2$V6/dat2$V8
reg25=felm(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2|0|V2,data = dat2)
summary(reg25)
reg26=felm(V1~V5+V11+I(V11*V5)+V8+V12+V13+poly(V4,5)|V3+V2|0|V2,data = dat2)
summary(reg26)
dat2$indyear=indyear
dat2$wage=wage
r25=feols(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2,data=dat2)
summary(r25,cluster=dat2$V2)
#level which makes no sense
reg20=felm(V1~V5+I(V14*V5)+V8+V12+V13|indyear+V2|0|V2,data = dat2)
summary(reg20)

v2=unique(dat2$V2)
dat2=dat2[(dat2$V2 %in% v2[which(tapply(dat2$V2,dat2$V2,length)>10)]),]

reg27=pmg(V1~V5+V11+V8+V12+V13,dat2,model = "mg",index = c("V2","V4"))
summary(reg27)

fe=getfe(reg25)
options(warn=2)
fes=c(1:length(dat2$V2))
i=1
for (x in dat2$V2){
  fes[i]=fe$effect[fe$fe=="V2" & fe$idx==x]
  i=i+1
}
feindy=c(1:length(dat2$indyear))
i=1
for (x in dat2$indyear){
  feindy[i]=fe$effect[fe$idx==x]
  i=i+1
}
dat2$fes=fes
dat2$feindy=feindy
wage=unique(dat2$V11)

#to see average firm growth rate vs growth of firm with TFP one std above 
#implied dispersion effect by the growth rates

avg=c(1:length(unique(dat2$V4)))*NaN
sdg=c(1:length(unique(dat2$V4)))*NaN
psdg=c(1:length(unique(dat2$V4)))*NaN
for (i in c(1:length(unique(dat2$V4)))){
  dat2_temp <- lp5r%>%
    filter(V4==1980+i-1)
  avg[i]=mean(dat2_temp$V1[which(abs(dat2_temp$V5-mean(dat2_temp$V5))<0.01)])
  sdg[i]=sd(dat2_temp$V5)
  ming=min(dat2_temp$V5)
  psdg[i]=sd((dat2_temp$V5-ming)*0.269/100+dat2_temp$V5)
}
mean(avg)
mean(psdg-sdg)/mean(sdg)
mean(diff(sdg)/sdg[1:30])





#predict using the regression model (which doesnt work very well)

dat2_80 <- dat2%>%
  filter(V4=="1980")



psd=c(1:(length(wage)))
for (i in c(1:length(wage))){
  psd[i]=sd(dat2_80$V5)
  pdzp=reg25$coefficients[1]*dat2_80$V5+
    dat2_80$fes
  dat2_80$V5=dat2_80$V5+pdzp
}

plot(asd,type = 'l')
lines(psd)

#predict using full sample
pzp=predict(r25)+dat2$V5
azp=dat2$V5+dat2$V1
pds=aggregate(pzp,FUN=sd,by=list(year=dat2$V4))
ads=aggregate(azp,FUN=sd,by=list(year=dat2$V4))
plot(ads,type="l",ylim=c(0.3,1.5))
lines(pds)
newd2=dat2
newd2[,"V11"]=0
pzp=predict(r25,newdata=newd2)+dat2$V5
pds=aggregate(pzp,FUN=std,by=list(year=dat2$V4))
lines(pds) 

v11=sum(unique(dat2$V11))
f80=dat2$V5[dat2$V4==1982]
pf80=f80*v11*r25$coefficients[2]
f10=dat2$V5[dat2$V4==2010 & dat2$V10<1981]
pf10=f10+dat2$V1[dat2$V4==2010 & dat2$V10<1981]
sd(pf80)
sd(pf10)
sd(f80)

# LP Horizon 2
lp2 <- read.csv("E:/firm project/data/lp2.csv", header=FALSE)
attach(lp2)
lp2=lp2[lp2$V4<2011,]
indyear=lp2$V3*10000+lp2$V4
reg1_lp2=felm(V1~V5+I(V5*V6)+V7+V8|indyear+V2|0|V2,data = lp2)
summary(reg1_lp2)
r10_lp2=plm(V1~V5+I(V5*V6)+V7+V8,data = lp2,index=c("V2","V4"),model = "within",effect="twoways")
summary(r10_lp2)


lp2r <- read.csv("E:/firm project/data/lp2r.csv", header=FALSE)
attach(lp2r)
lp2r=lp2r[lp2r$V4<2011,]
#lp2r=lp2r[lp2r$V10<1991,]
indyear=lp2r$V3*10000+lp2r$V4
reg21_lp2=felm(V1~V5+V6+V8+V12+V13|indyear+V2|0|V2,data = lp2r)
summary(reg21_lp2)
reg22_lp2=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp2r)
summary(reg22_lp2)
reg23_lp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp2r)
summary(reg23_lp2)
reg24_lp2=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V3,data = lp2r)
summary(reg24_lp2)
wage=lp2r$V6/lp2r$V8
reg25_lp2=felm(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2|0|V2,data = lp2r)
summary(reg25_lp2)
reg26_lp2=felm(V1~V5+V11+I(V11*V5)+V8+V12+V13+poly(V4,5)|V3+V2|0|V2,data = lp2r)
summary(reg26_lp2)

lp2r$me=reg26_lp2$coefficients[2]+reg26_lp2$coefficients[3]*lp2r$V5
plot(lp2r$V5,lp2r$me, ylab = "Marginal Effect of w",xlab = "log TFP Level",cex.axis=2,cex.lab=2)
abline(h=0,lwd = 3,col="red")
box(lwd=2)

v2=unique(lp2r$V2)
lp2r=lp2r[(lp2r$V2 %in% v2[which(tapply(lp2r$V2,lp2r$V2,length)>10)]),]

reg27_lp2=pmg(V1~V5+V11+V8+V12+V13,lp2r,model = "mg",index = c("V2","V4"))
summary(reg27_lp2)


lp2r$indyear=indyear
lp2r$wage=wage
r25_lp2=feols(V1~V5+I(V11*V5)+V8+V12+V13|V4+V2,data=lp2r)
summary(r25_lp2,cluster=lp2r$V2)


fe=getfe(reg25_lp2)
options(warn=2)
fes=c(1:length(lp2r$V2))
i=1
for (x in lp2r$V2){
  fes[i]=fe$effect[fe$fe=="V2" & fe$idx==x]
  i=i+1
}
pzp=predict(r25_lp2)+lp2r$V5-fes
azp=lp2r$V5+lp2r$V1
pds=aggregate(pzp,FUN=std,by=list(year=lp2r$V4))
ads=aggregate(azp,FUN=std,by=list(year=lp2r$V4))
plot(ads,type="l",ylim=c(0.3,1.5))
lines(pds)
newd2=lp2r
newd2[,c("V8","V12","V13")]=0
pzp1=predict(r25_lp2,newdata=newd2)+lp2r$V5
pds1=aggregate(pzp1,FUN=std,by=list(year=lp2r$V4))
lines(pds1)


# LP Horizon 3
lp3 <- read.csv("E:/firm project/data/lp3.csv", header=FALSE)
attach(lp3)
indyear=lp3$V3*10000+lp3$V4
reg1_lp3=felm(V1~V5+I(V5*V6)+V7+V8|indyear+V2|0|V2,data = lp3)
summary(reg1_lp3)

lp3r <- read.csv("E:/firm project/data/lp3r.csv", header=FALSE)
attach(lp3r)
lp3r=lp3r[lp3r$V4<2011,]
indyear=lp3r$V3*10000+lp3r$V4
reg21_lp3=felm(V1~V5+V6+V8+V12+V13|indyear+V2|0|V2,data = lp3r)
summary(reg21_lp3)
reg22_lp3=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp3r)
summary(reg22_lp3)
reg23_lp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp3r)
summary(reg23_lp3)
reg24_lp3=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp3r)
summary(reg24_lp3)
wage=lp3r$V6/lp3r$V8
reg25_lp3=felm(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2|0|V2,data = lp3r)
summary(reg25_lp3)
reg26_lp3=felm(V1~V5+V11+I(V11*V5)+V8+V12+V13+poly(V4,5)|V2+V3|0|V2,data = lp3r)
summary(reg26_lp3)
lp3r$indyear=indyear
#lp3r$wage=wage
r25_lp3=feols(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2,data=lp3r)
summary(r25_lp3,cluster=lp3r$V2)

v2=unique(lp3r$V2)
lp3r=lp3r[(lp3r$V2 %in% v2[which(tapply(lp3r$V2,lp3r$V2,length)>10)]),]

reg27_lp3=pmg(V1~V5+V11+V8+V12+V13,lp3r,model = "mg",index = c("V2","V4"))
summary(reg27_lp3)

# LP Horizon 4
lp4<- read.csv("E:/firm project/data/lp4.csv", header=FALSE)
attach(lp4)

indyear=lp4$V3*10000+lp4$V4
reg1_lp4=felm(V1~V5+I(V5*V6)+V7+V8|indyear+V2|0|V2,data = lp4)
summary(reg1_lp4)

lp4r <- read.csv("E:/firm project/data/lp4r.csv", header=FALSE)
attach(lp4r)
lp4r=lp4r[lp4r$V4<2011,]
indyear=lp4r$V3*10000+lp4r$V4
reg21_lp4=felm(V1~V5+V6+V8+V12+V13|indyear+V2|0|V2,data = lp4r)
summary(reg21_lp4)
reg22_lp4=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp4r)
summary(reg22_lp4)
reg23_lp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp4r)
summary(reg23_lp4)
reg24_lp4=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp4r)
summary(reg24_lp4)
wage=lp4r$V6/lp4r$V8
reg25_lp4=felm(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2|0|V2,data = lp4r)
summary(reg25_lp4)
reg26_lp4=felm(V1~V5+V11+I(V11*V5)+V8+V12+V13+poly(V4,5)|V3+V2|0|V2,data = lp4r)
summary(reg26_lp4)
lp4r$indyear=indyear
#lp4r$wage=wage
r25_lp4=feols(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2,data=lp4r)
summary(r25_lp4,cluster=lp4r$V2)

v2=unique(lp4r$V2)
lp4r=lp4r[(lp4r$V2 %in% v2[which(tapply(lp4r$V2,lp4r$V2,length)>10)]),]

reg27_lp4=pmg(V1~V5+V11+V8+V12+V13,lp4r,model = "mg",index = c("V2","V4"))
summary(reg27_lp4)


# LP Horizon 5
lp5<- read.csv("E:/firm project/data/lp5.csv", header=FALSE)
attach(lp5)
indyear=lp5$V3*10000+lp5$V4
reg1_lp5=felm(V1~V5+I(V5*V6)+V7+V8|indyear+V2|0|0,data = lp5)
summary(reg1_lp5)

lp5r <- read.csv("E:/firm project/data/lp5r.csv", header=FALSE)
attach(lp5r)
indyear=lp5r$V3*10000+lp5r$V4
reg21_lp5=felm(V1~V5+V6+V8+V12+V13|indyear+V2|0|V2,data = lp5r)
summary(reg21_lp5)
reg22_lp5=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp5r)
summary(reg22_lp5)
reg23_lp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp5r)
summary(reg23_lp5)
reg24_lp5=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp5r)
summary(reg24_lp5)
wage=lp5r$V6/lp5r$V8
reg25_lp5=felm(V1~V5+I(V11*V5)+V8+V12+V13|indyear+V2|0|V2,data = lp5r)
summary(reg25_lp5)
reg26_lp5=felm(V1~V5+V11+I(V11*V5)+V8+V12+V13+poly(V4,5)|V2+V3|0|V2,data = lp5r)
summary(reg26_lp5)
lp5r$indyear=indyear
lp5r$wage=wage
r25_lp5=feols(V1~V5+I(V11*V5)+V8|indyear+V2,data=lp5r)
summary(r25_lp5,cluster=lp5r$V2)

v2=unique(lp5r$V2)
lp5r=lp5r[(lp5r$V2 %in% v2[which(tapply(lp5r$V2,lp5r$V2,length)>10)]),]

reg27_lp5=pmg(V1~V5+V11+V8+V12+V13,lp5r,model = "mg",index = c("V2","V4"))
summary(reg27_lp5)


#LP Placebo H1
pdat1 <- read.csv("E:/firm project/data/pdat1.csv", header=FALSE)

attach(pdat1)

indyear=pdat1$V3*10000+pdat1$V4
preg1=felm(V1~V5+I(V5*V6)|V2|0|0,data = pdat1)
summary(preg1)

pdat2 <- read.csv("E:/firm project/data/pdat2.csv", header=FALSE)
attach(pdat2)
pdat2=pdat2[pdat2$V4<2011,]
indyear=pdat2$V3*10000+pdat2$V4
preg21=felm(V1~V5+V6+I(V5*V6)+V8+V10+V12+V13|indyear|0|V3,data = pdat2)
summary(preg21)
preg22=felm(V1~V5+V7+I(V5*V7)+V9+V10+V12+V13|indyear|0|V3,data = pdat2)
summary(preg22)
preg23=felm(V1~V5+V6+I(V5*V6)+V8+V10+V12+V13|V3+V4|0|V3,data = pdat2)
summary(reg23)
preg24=felm(V1~V5+V7+I(V5*V7)+V9+V12+V13|V2+V4|0|V2,data = pdat2)
summary(preg24)
wage=pdat2$V6/pdat2$V8
preg25=felm(V1~V5+I(wage*V5)+V8+V12+V13|indyear+V2|0|V2,data = pdat2)
summary(preg25)
preg26=felm(V1~V5+wage+I(wage*V5)+V8+V4+V12+V13|V3+V2|0|V2,data = pdat2)
summary(preg26)

#LP Placebo H2
plp2 <- read.csv("E:/firm project/data/plp2.csv", header=FALSE)
attach(plp2)
indyear=plp2$V3*10000+plp2$V4
reg1_plp2=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp2)
summary(reg1_plp2)

plp2r <- read.csv("E:/firm project/data/plp2r.csv", header=FALSE)
attach(plp2r)
plp2r=plp2r[plp2r$V4<2011,]
indyear=plp2r$V3*10000+plp2r$V4
reg21_plp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp2r)
summary(reg21_plp2)
reg22_plp2=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp2r)
summary(reg22_plp2)
reg23_plp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp2r)
summary(reg23_plp2)
reg24_plp2=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp2r)
summary(reg24_plp2)
wage=plp2r$V6/plp2r$V8
reg25_plp2=felm(V1~V5+I(wage*V5)+V8|indyear+V2|0|V2,data = plp2r)
summary(reg25_plp2)
preg26_plp2=felm(V1~V5+wage+I(wage*V5)+V8+V4|V3+V2|0|V2,data = plp2r)
summary(preg26_plp2)


#LP Placebo H3
plp3 <- read.csv("E:/firm project/data/plp3.csv", header=FALSE)
attach(plp3)
indyear=plp3$V3*10000+plp3$V4
reg1_plp3=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp3)
summary(reg1_plp3)

plp3r <- read.csv("E:/firm project/data/plp3r.csv", header=FALSE)
attach(plp3r)
plp3r=plp3r[plp3r$V4<2011,]
indyear=plp3r$V3*10000+plp3r$V4
reg21_plp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp3r)
summary(reg21_plp3)
reg22_plp3=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp3r)
summary(reg22_plp3)
reg23_plp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp3r)
summary(reg23_plp3)
reg24_plp3=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp3r)
summary(reg24_plp3)
wage=plp3r$V6/plp3r$V8
reg25_plp3=felm(V1~V5+I(wage*V5)+V8|indyear+V2|0|V2,data = plp3r)
summary(reg25_plp3)
preg26_plp3=felm(V1~V5+wage+I(wage*V5)+V8+V4|V3+V2|0|V2,data = plp3r)
summary(preg26_plp3)


#LP Placebo H4
plp4 <- read.csv("E:/firm project/data/plp4.csv", header=FALSE)
attach(plp4)
indyear=plp4$V3*10000+plp4$V4
reg1_plp4=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp4)
summary(reg1_plp4)

plp4r <- read.csv("E:/firm project/data/plp4r.csv", header=FALSE)
attach(plp4r)
plp4r=plp4r[plp4r$V4<2011,]
indyear=plp4r$V3*10000+plp4r$V4
reg21_plp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp4r)
summary(reg21_plp4)
reg22_plp4=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp4r)
summary(reg22_plp4)
reg23_plp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp4r)
summary(reg23_plp4)
reg24_plp4=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp4r)
summary(reg24_plp4)
wage=plp4r$V6/plp4r$V8
reg25_plp4=felm(V1~V5+I(wage*V5)+V8|indyear+V2|0|V2,data = plp4r)
summary(reg25_plp4)
preg26_plp4=felm(V1~V5+wage+I(wage*V5)+V8+V4|V3+V2|0|V2,data = plp4r)
summary(preg26_plp4)

#LP Placebo H5
plp5 <- read.csv("E:/firm project/data/plp5.csv", header=FALSE)
attach(plp5)
indyear=plp5$V3*10000+plp5$V4
reg1_plp5=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp5)
summary(reg1_plp5)

plp5r <- read.csv("E:/firm project/data/plp5r.csv", header=FALSE)
attach(plp5r)
plp5r=plp5r[plp5r$V4<2011,]
indyear=plp5r$V3*10000+plp5r$V4
reg21_plp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp5r)
summary(reg21_plp5)
reg22_plp5=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp5r)
summary(reg22_plp5)
reg23_plp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp5r)
summary(reg23_plp5)
reg24_plp5=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp5r)
summary(reg24_plp5)
wage=plp5r$V6/plp5r$V8
reg25_plp5=felm(V1~V5+I(wage*V5)+V8|indyear+V2|0|V2,data = plp5r)
summary(reg25_plp5)
preg26_plp5=felm(V1~V5+wage+I(wage*V5)+V8+V4|V3+V2|0|V2,data = plp5r)
summary(preg26_plp5)

#irf
irf_mean = matrix(NaN, 1, 5)
irf_up = matrix(NaN, 1, 5)
irf_low = matrix(NaN, 1, 5)

confint=1.96
irf_mean[[1,1]]=reg21$coefficients[3]
irf_up[[1,1]]=reg21$coefficients[3]+confint*reg21$cse[3]
irf_low[[1,1]]=reg21$coefficients[3]-confint*reg21$cse[3]

irf_mean[[1,2]]=reg21_lp2$coefficients[3]
irf_up[[1,2]]=reg21_lp2$coefficients[3]+confint*reg21_lp2$cse[3]
irf_low[[1,2]]=reg21_lp2$coefficients[3]-confint*reg21_lp2$cse[3]

irf_mean[[1,3]]=reg21_lp3$coefficients[3]
irf_up[[1,3]]=reg21_lp3$coefficients[3]+confint*reg21_lp3$cse[3]
irf_low[[1,3]]=reg21_lp3$coefficients[3]-confint*reg21_lp3$cse[3]

irf_mean[[1,4]]=reg21_lp4$coefficients[3]
irf_up[[1,4]]=reg21_lp4$coefficients[3]+confint*reg21_lp4$cse[3]
irf_low[[1,4]]=reg21_lp4$coefficients[3]-confint*reg21_lp4$cse[3]


irf_mean[[1,5]]=reg21_lp5$coefficients[3]
irf_up[[1,5]]=reg21_lp5$coefficients[3]+confint*reg21_lp5$cse[3]
irf_low[[1,5]]=reg21_lp5$coefficients[3]-confint*reg21_lp5$cse[3]

irf = list(irf_mean       = irf_mean,
               irf_low    = irf_low,
               irf_up     = irf_up)

plot(irf$irf_low, type="n", ylim = c(-0.1, 0.2), xlim = c(1,5),
     ylab = "%", xlab = "Year",cex.axis=1.5,cex.lab=1.5)



# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf$irf_low), length(irf$irf_low):1),
  c(irf$irf_up, rev(irf$irf_low)), 
  col = "grey80", border = NA)

# add coefficient estimate line
lines(1:5,irf$irf_mean, col = "red",lwd = 4)
abline(h=0,lwd = 2)


box(lwd=2)

#not rnd adjusted

irf_mean = c(1:5)*NaN
irf_up = c(1:5)*NaN
irf_low = c(1:5)*NaN

confint=1.96
irf_mean[1]=reg25$coefficients[2]
irf_up[1]=reg25$coefficients[2]+confint*reg25$cse[2]
irf_low[1]=reg25$coefficients[2]-confint*reg25$cse[2]

irf_mean[2]=reg25_lp2$coefficients[2]
irf_up[2]=reg25_lp2$coefficients[2]+confint*reg25_lp2$cse[2]
irf_low[2]=reg25_lp2$coefficients[2]-confint*reg25_lp2$cse[2]

irf_mean[3]=reg25_lp3$coefficients[2]
irf_up[3]=reg25_lp3$coefficients[2]+confint*reg25_lp3$cse[2]
irf_low[3]=reg25_lp3$coefficients[2]-confint*reg25_lp3$cse[2]

irf_mean[4]=reg25_lp4$coefficients[2]
irf_up[4]=reg25_lp4$coefficients[2]+confint*reg25_lp4$cse[2]
irf_low[4]=reg25_lp4$coefficients[2]-confint*reg25_lp4$cse[2]


irf_mean[5]=reg25_lp5$coefficients[2]
irf_up[5]=reg25_lp5$coefficients[2]+confint*reg25_lp5$cse[2]
irf_low[5]=reg25_lp5$coefficients[2]-confint*reg25_lp5$cse[2]




irf = tibble(irf_mean,irf_low,irf_up)
irf=irf %>% 
  mutate(horizon=c(1:5))

irf$horizon=factor(irf$horizon)

ggplot(irf)+
  geom_errorbar(aes(ymin=irf_low,ymax=irf_up, x=horizon),
                stat="identity",width = 0.3,size=2)+
  ylim(-1.5, 2)+
  geom_point(aes(y=irf_mean,x=horizon),shape=3, size=6, fill="black",stroke = 2)+
  geom_hline(yintercept=0,linetype="dashed", color = "red",size=1)+
  theme_classic(base_size = 30)+
  ylab(TeX("$\\gamma^h$"))


#placebo

irf_mean = matrix(NaN, 1, 5)
irf_up = matrix(NaN, 1, 5)
irf_low = matrix(NaN, 1, 5)

confint=1.96
irf_mean[[1,1]]=preg21$coefficients[3]
irf_up[[1,1]]=preg21$coefficients[3]+confint*preg21$cse[3]
irf_low[[1,1]]=preg21$coefficients[3]-confint*preg21$cse[3]

irf_mean[[1,2]]=reg21_plp2$coefficients[3]
irf_up[[1,2]]=reg21_plp2$coefficients[3]+confint*reg21_plp2$cse[3]
irf_low[[1,2]]=reg21_plp2$coefficients[3]-confint*reg21_plp2$cse[3]

irf_mean[[1,3]]=reg21_plp3$coefficients[3]
irf_up[[1,3]]=reg21_plp3$coefficients[3]+confint*reg21_plp3$cse[3]
irf_low[[1,3]]=reg21_plp3$coefficients[3]-confint*reg21_plp3$cse[3]

irf_mean[[1,4]]=reg21_plp4$coefficients[3]
irf_up[[1,4]]=reg21_plp4$coefficients[3]+confint*reg21_plp4$cse[3]
irf_low[[1,4]]=reg21_plp4$coefficients[3]-confint*reg21_plp4$cse[3]


irf_mean[[1,5]]=reg21_plp5$coefficients[3]
irf_up[[1,5]]=reg21_plp5$coefficients[3]+confint*reg21_plp5$cse[3]
irf_low[[1,5]]=reg21_plp5$coefficients[3]-confint*reg21_plp5$cse[3]

irf = list(irf_mean       = irf_mean,
           irf_low    = irf_low,
           irf_up     = irf_up)

plot(irf$irf_low, type="n", ylim = c(-0.5, 0.5), xlim = c(1,5),
     ylab = "%", xlab = "Year",cex.axis=1.5, cex.lab=1.5)



# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf$irf_low), length(irf$irf_low):1),
  c(irf$irf_up, rev(irf$irf_low)), 
  col = "grey80", border = NA)

# add coefficient estimate line
lines(1:5,irf$irf_mean, col = "red",lwd = 4)
abline(h=0,lwd = 2)


box(lwd=2)

#not rnd adjusted

irf_mean = c(1:5)*NaN
irf_up = c(1:5)*NaN
irf_low = c(1:5)*NaN

confint=1.96
irf_mean[1]=preg25$coefficients[2]
irf_up[1]=preg25$coefficients[2]+confint*preg25$cse[2]
irf_low[1]=preg25$coefficients[2]-confint*preg25$cse[2]

irf_mean[2]=reg25_plp2$coefficients[2]
irf_up[2]=reg25_plp2$coefficients[2]+confint*reg25_plp2$cse[2]
irf_low[2]=reg25_plp2$coefficients[2]-confint*reg25_plp2$cse[2]

irf_mean[3]=reg25_plp3$coefficients[2]
irf_up[3]=reg25_plp3$coefficients[2]+confint*reg25_plp3$cse[2]
irf_low[3]=reg25_plp3$coefficients[2]-confint*reg25_plp3$cse[2]

irf_mean[4]=reg25_plp4$coefficients[2]
irf_up[4]=reg25_plp4$coefficients[2]+confint*reg25_plp4$cse[2]
irf_low[4]=reg25_plp4$coefficients[2]-confint*reg25_plp4$cse[2]


irf_mean[5]=reg25_plp5$coefficients[2]
irf_up[5]=reg25_plp5$coefficients[2]+confint*reg25_plp5$cse[2]
irf_low[5]=reg25_plp5$coefficients[2]-confint*reg25_plp5$cse[2]

irf = tibble(irf_mean,irf_low,irf_up)
irf=irf %>% 
  mutate(horizon=c(1:5))

irf$horizon=factor(irf$horizon)

ggplot(irf)+
  geom_errorbar(aes(ymin=irf_low,ymax=irf_up, x=horizon),
                stat="identity",width = 0.3,size=2)+
  ylim(-1.5, 2)+
  geom_point(aes(y=irf_mean,x=horizon),shape=3, size=6, fill="black",stroke = 2)+
  geom_hline(yintercept=0,linetype="dashed", color = "red",size=1)+
  theme_classic(base_size = 30)+
  ylab(TeX("$\\gamma^h$"))

#prediction
#for dat1

dat1$zp0=dat1$V5+dat1$V1
dat1=setDT(dat1)[, mzp0 := mean(zp0), by = .(V3,V4)][]
dzp=dat1$zp0-dat1$mzp0
dat1$dzp=dzp
std0=aggregate(dat1$dzp,FUN=std,by=list(year=dat1$V4))


#z=predict(r25)
b1=reg1$coefficients[1]
b2=reg1$coefficients[2]
b3=reg1$coefficients[3]
fe=getfe(reg1)
fes=c(1:length(dat1$indyear))*0
i=1
for (x in dat1$indyear){
  fes[i]=fe$effect[fe$idx==x]
  i=i+1
}

z=dat1$V5*dat1$V6*b2#+fes
#z=predict(r25)-dat1$V5*b1-dat1$V8*b3
zp1=z+dat1$V5
year1=dat1$V4+1
dat1$zp1=zp1
dat1$year1=year1
dat1=setDT(dat1)[,mzp1 := mean(zp1), by= .(V3,year1)][]
dzp1=dat1$zp1-dat1$mzp1
dat1$dzp1=dzp1
std1=aggregate(dat1$dzp1,FUN=std,by=list(year=dat1$year1))
pred1=(std1[,2]-std0[,2])/std0[,2]
real1=diff(std0[,2])/std0[c(1:length(std0)-1),2]

 
dat2=setDT(dat2)[, mzp0 := mean(V5), by = .(V3,V4)][]
dzp=dat2$V5-dat2$mzp0
dat2$dzp=dzp
std0=aggregate(dat2$dzp,FUN=std,by=list(year=dat2$V4))
m0=aggregate(dat2$V5,FUN=mean,by=list(year=dat2$V4))

dat2$zp0=dat2$V5+dat2$V1
dat2=setDT(dat2)[, mzp00 := mean(dat2$zp0), by = .(V3,V4)][]
dat2$dzp1=dat2$zp0-dat2$mzp00
pstd0=aggregate(dat2$dzp1,FUN=std,by=list(year=dat2$V4))

#z=predict(r25)
b1=r25$coefficients[1]
b2=r25$coefficients[2]
b3=r25$coefficients[3]
fe=getfe(reg25)
fes=c(1:length(dat2$indyear))*0
i=1
for (x in dat2$indyear){
  fes[i]=fe$effect[fe$idx==x]
  i=i+1
}
  
z=dat2$V5*dat2$V11*b2#+fes
#z=predict(r25)-dat2$V5*b1-dat2$V8*b3
zp1=z+dat2$V5
year1=dat2$V4+1
dat2$zp1=zp1
dat2$year1=year1
dat2=setDT(dat2)[,mzp1 := mean(zp1), by= .(V3,year1)][]
dzp1=dat2$zp1-dat2$mzp1
dat2$dzp1=dzp1
std1=aggregate(dat2$dzp1,FUN=std,by=list(year=dat2$year1))
pred1=(std1[,2]-std0[,2])/std0[,2]
real1=(pstd0[,2]-std0[,2])/std0[,2]

#LP2
#lp2r=setDT(lp2r)[, mzp0 := lapply(lp2r$V5,mean), by = list(lp2r$V3,lp2r$V4)]
mzp0=lp2r %>%
  group_by(V3,V4) %>%
  summarise(mzp0=mean(V5)) 
lp2r=lp2r %>%
  left_join(mzp0)

dzp=lp2r$V5-lp2r$mzp0
lp2r$dzp=dzp
std0=aggregate(lp2r$dzp,FUN=std,by=list(lp2r$V4))

year2=lp2r$V4+2
lp2r$year2=year2
lp2r$zp0=lp2r$V5+lp2r$V1
mzp00=lp2r %>%
  group_by(V3,V4) %>%
  summarise(mzp00=mean(zp0)) 
lp2r=lp2r %>%
  left_join(mzp00)

#lp2r=setDT(lp2r)[, mzp00 := mean(lp2r$zp0), by = .(V3,year2)]
lp2r$dzp1=lp2r$zp0-lp2r$mzp00
pstd2=aggregate(lp2r$dzp1,FUN=IQR,by=list(year=lp2r$year2))

b1=reg25_lp2$coefficients[2]
z=lp2r$V5*lp2r$V11*b1
zp2=z+lp2r$V5

lp2r$zp2=zp2
mzp2=lp2r %>%
  group_by(V3,V4) %>%
  summarise(mzp2=mean(zp2)) 
lp2r=lp2r %>%
  left_join(mzp2)
#lp2r=setDT(lp2r)[,mzp2 := mean(lp2r$zp2), by= .(lp2r$V3,lp2r$year2)][]
lp2r$dzp2=lp2r$zp2-lp2r$mzp2
std2=aggregate(lp2r$dzp2,FUN=IQR,by=list(lp2r$year2))
pred2=(std2[,2]-std0[,2])/std0[,2]
real2=(pstd2[,2]-std0[,2])/std0[,2]
mean(pred2)
mean(real2)
mean(pred2)/mean(real2)

#LP3
lp3r=setDT(lp3r)[, mzp0 := mean(V5), by = .(V3,V4)][]
dzp=lp3r$V5-lp3r$mzp0
lp3r$dzp=dzp
std0=aggregate(lp3r$dzp,FUN=std,by=list(lp3r$V4))

lp3r$zp0=lp3r$V5+lp3r$V1
lp3r=setDT(lp3r)[, mzp00 := mean(zp0), by = .(V3,V4)][]
lp3r$dzp1=lp3r$zp0-lp3r$mzp00
pstd3=aggregate(lp3r$dzp1,FUN=std,by=list(year=lp3r$V4))

b1=r25_lp3$coefficients[2]
z=lp3r$V5*lp3r$V11*b1
zp3=z+lp3r$V5
year3=lp3r$V4+3
lp3r$zp3=zp3
lp3r$year3=year3
lp3r=setDT(lp3r)[,mzp3 := mean(zp3), by= .(V3,year3)][]
lp3r$dzp3=lp3r$zp3-lp3r$mzp3
std3=aggregate(lp3r$dzp3,FUN=std,by=list(lp3r$year3))
pred3=(std3[,2]-std0[,2])/std0[,2]
real3=(pstd3[,2]-std0[,2])/std0[,2]

#LP4
lp4r=setDT(lp4r)[, mzp0 := mean(V5), by = .(V3,V4)][]
dzp=lp4r$V5-lp4r$mzp0
lp4r$dzp=dzp
std0=aggregate(lp4r$dzp,FUN=std,by=list(lp4r$V4))

b1=r25_lp4$coefficients[2]
z=lp4r$V5*lp4r$V11*b1
zp4=z+lp4r$V5
year4=lp4r$V4+4
lp4r$zp4=zp4
lp4r$year4=year4
lp4r=setDT(lp4r)[,mzp4 := mean(zp4), by= .(V3,year4)][]
lp4r$dzp4=lp4r$zp4-lp4r$mzp4
std4=aggregate(lp4r$dzp4,FUN=std,by=list(lp4r$year4))
pred4=(std4[,2]-std0[,2])
real4=diff(std0[,2],lag=4)


#marginal effect vs TFP level
p1=ggplot()+
  geom_density(aes(x=dat2$V5),size=1,fill="grey")+
  xlim(8,16)+
  xlab("log TFP (LP1)")+
  geom_line(aes(y=reg26$coefficients[2]+reg26$coefficients[3]*dat2$V5,x=dat2$V5),size=1,color="red",linetype = "dashed")+
  scale_y_continuous(
    # Features of the first axis
    limits=c(-1,1),name = "Firm TFP Density",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1)
  ) +
  theme_classic(base_size = 20)+
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        
  ) 

p2=ggplot()+
  geom_density(aes(x=lp2r$V5),size=1,fill="grey")+
  xlim(8,16)+
  xlab("log TFP (LP2)")+
  geom_line(aes(y=reg26_lp2$coefficients[2]+reg26_lp2$coefficients[3]*lp2r$V5,x=lp2r$V5),size=1,color="red",linetype = "dashed")+
  scale_y_continuous(
    # Features of the first axis
    limits=c(-1,1),name = " ",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1,name="Marginal Effect")
  ) +
  theme_classic(base_size = 20)+
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  ) 
  
p3=ggplot()+
  geom_density(aes(x=lp3r$V5),size=1,fill="grey")+
  xlim(8,16)+
  xlab("log TFP (LP3)")+
  geom_line(aes(y=reg26_lp3$coefficients[2]+reg26_lp3$coefficients[3]*lp3r$V5,x=lp3r$V5),size=1,color="red",linetype = "dashed")+
  scale_y_continuous(
    # Features of the first axis
    limits=c(-1,1),name = "Firm TFP Density",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1)
  ) +
  theme_classic(base_size = 20)+
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
  ) 

p4=ggplot()+
  geom_density(aes(x=lp4r$V5),size=1,fill="grey")+
  xlim(8,16)+
  xlab("log TFP (LP4)")+
  geom_line(aes(y=reg26_lp4$coefficients[2]+reg26_lp4$coefficients[3]*lp4r$V5,x=lp4r$V5),size=1,color="red",linetype = "dashed")+
  scale_y_continuous(
    # Features of the first axis
    limits=c(-1,1),name = " ",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1,name="Marginal Effect")
  ) +
  theme_classic(base_size = 20)+
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  ) 

grid.arrange(p1, p2,p3, p4, nrow = 2)


#RND Dynamics (not ideal result)

datrnd <- read.csv("E:/firm project/data/datrnd3.csv", header=FALSE)
attach(datrnd)
datrnd=datrnd[datrnd$V4<2011,]
indyear=datrnd$V3*10000+datrnd$V4
regrnd1=felm(V1~V5+V6+V7+I(V9*V5)+V10+V11|indyear+V2|0|V2,data = datrnd)
summary(regrnd1)





#industry level regression
#disp change
idat1 <- read.csv("E:/firm project/agg data/idat1.csv", header=FALSE)
attach(idat1)
colnames(idat1)=c("DDisp","id","year","Dkappa","Emp","DTFP")
reg25=felm(DDisp~V4+V6+V5|V2+V3|0|V2,idat1)
summary(reg25)
#disp level
idat2 <- read.csv("E:/firm project/agg data/idat2.csv", header=FALSE)
attach(idat2)
colnames(idat2)=c("Disp","id","year","kappa","DTFP","Emp")
reg26=felm(Disp~V4+V5+V6|V2+V3|0|V2,idat2)
summary(reg26)

stargazer(reg25,reg26)


