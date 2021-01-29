library(lfe)
library(sandwich)
library(lmtest)
library(plm)
library(ggplot2)
library(pracma)
library(stargazer)
library(plm)

#LP Horizon 1
dat1 <- read.csv("E:/firm project/data/dat1.csv", header=FALSE)

attach(dat1)

indyear=dat1$V3*10000+dat1$V4
reg1=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = dat1)
summary(reg1)

dat2 <- read.csv("E:/firm project/data/dat2.csv", header=FALSE)
attach(dat2)
indyear=dat2$V3*10000+dat2$V4
reg21=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = dat2)
summary(reg21)
reg22=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = dat2)
summary(reg22)
reg23=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = dat2)
summary(reg23)
reg24=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = dat2)
summary(reg24)

# LP Horizon 2
lp2 <- read.csv("E:/firm project/data/lp2.csv", header=FALSE)
attach(lp2)
indyear=lp2$V3*10000+lp2$V4
reg1_lp2=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = lp2)
summary(reg1_lp2)

lp2r <- read.csv("E:/firm project/data/lp2r.csv", header=FALSE)
attach(lp2r)
indyear=lp2r$V3*10000+lp2r$V4
reg21_lp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = lp2r)
summary(reg21_lp2)
reg22_lp2=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp2r)
summary(reg22_lp2)
reg23_lp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp2r)
summary(reg23_lp2)
reg24_lp2=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp2r)
summary(reg24_lp2)

# LP Horizon 3
lp3 <- read.csv("E:/firm project/data/lp3.csv", header=FALSE)
attach(lp3)
indyear=lp3$V3*10000+lp3$V4
reg1_lp3=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = lp3)
summary(reg1_lp3)

lp3r <- read.csv("E:/firm project/data/lp3r.csv", header=FALSE)
attach(lp3r)
indyear=lp3r$V3*10000+lp3r$V4
reg21_lp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = lp3r)
summary(reg21_lp3)
reg22_lp3=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp3r)
summary(reg22_lp3)
reg23_lp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp3r)
summary(reg23_lp3)
reg24_lp3=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp3r)
summary(reg24_lp3)

# LP Horizon 4
lp4<- read.csv("E:/firm project/data/lp4.csv", header=FALSE)
attach(lp4)
indyear=lp4$V3*10000+lp4$V4
reg1_lp4=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = lp4)
summary(reg1_lp4)

lp4r <- read.csv("E:/firm project/data/lp4r.csv", header=FALSE)
attach(lp4r)
indyear=lp4r$V3*10000+lp4r$V4
reg21_lp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = lp4r)
summary(reg21_lp4)
reg22_lp4=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp4r)
summary(reg22_lp4)
reg23_lp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp4r)
summary(reg23_lp4)
reg24_lp4=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp4r)
summary(reg24_lp4)

# LP Horizon 5
lp5<- read.csv("E:/firm project/data/lp5.csv", header=FALSE)
attach(lp5)
indyear=lp5$V3*10000+lp5$V4
reg1_lp5=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = lp5)
summary(reg1_lp5)

lp5r <- read.csv("E:/firm project/data/lp5r.csv", header=FALSE)
attach(lp5r)
indyear=lp5r$V3*10000+lp5r$V4
reg21_lp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = lp5r)
summary(reg21_lp5)
reg22_lp5=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = lp5r)
summary(reg22_lp5)
reg23_lp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = lp5r)
summary(reg23_lp5)
reg24_lp5=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = lp5r)
summary(reg24_lp5)


#LP Placebo H1
pdat1 <- read.csv("E:/firm project/data/pdat1.csv", header=FALSE)

attach(pdat1)

indyear=pdat1$V3*10000+pdat1$V4
preg1=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = pdat1)
summary(preg1)

pdat2 <- read.csv("E:/firm project/data/pdat2.csv", header=FALSE)
attach(pdat2)
indyear=pdat2$V3*10000+pdat2$V4
preg21=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = pdat2)
summary(preg21)
preg22=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = pdat2)
summary(preg22)
preg23=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = pdat2)
summary(reg23)
preg24=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = pdat2)
summary(preg24)

#LP Placebo H2
plp2 <- read.csv("E:/firm project/data/plp2.csv", header=FALSE)
attach(plp2)
indyear=plp2$V3*10000+plp2$V4
reg1_plp2=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp2)
summary(reg1_plp2)

plp2r <- read.csv("E:/firm project/data/plp2r.csv", header=FALSE)
attach(plp2r)
indyear=plp2r$V3*10000+plp2r$V4
reg21_plp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp2r)
summary(reg21_plp2)
reg22_plp2=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp2r)
summary(reg22_plp2)
reg23_plp2=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp2r)
summary(reg23_plp2)
reg24_plp2=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp2r)
summary(reg24_plp2)

#LP Placebo H3
plp3 <- read.csv("E:/firm project/data/plp3.csv", header=FALSE)
attach(plp3)
indyear=plp3$V3*10000+plp3$V4
reg1_plp3=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp3)
summary(reg1_plp3)

plp3r <- read.csv("E:/firm project/data/plp3r.csv", header=FALSE)
attach(plp3r)
indyear=plp3r$V3*10000+plp3r$V4
reg21_plp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp3r)
summary(reg21_plp3)
reg22_plp3=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp3r)
summary(reg22_plp3)
reg23_plp3=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp3r)
summary(reg23_plp3)
reg24_plp3=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp3r)
summary(reg24_plp3)

#LP Placebo H4
plp4 <- read.csv("E:/firm project/data/plp4.csv", header=FALSE)
attach(plp4)
indyear=plp4$V3*10000+plp4$V4
reg1_plp4=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp4)
summary(reg1_plp4)

plp4r <- read.csv("E:/firm project/data/plp4r.csv", header=FALSE)
attach(plp4r)
indyear=plp4r$V3*10000+plp4r$V4
reg21_plp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp4r)
summary(reg21_plp4)
reg22_plp4=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp4r)
summary(reg22_plp4)
reg23_plp4=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp4r)
summary(reg23_plp4)
reg24_plp4=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp4r)
summary(reg24_plp4)

#LP Placebo H5
plp5 <- read.csv("E:/firm project/data/plp5.csv", header=FALSE)
attach(plp5)
indyear=plp5$V3*10000+plp5$V4
reg1_plp5=felm(V1~V5+I(V5*V6)|indyear+V2|0|0,data = plp5)
summary(reg1_plp5)

plp5r <- read.csv("E:/firm project/data/plp5r.csv", header=FALSE)
attach(plp5r)
indyear=plp5r$V3*10000+plp5r$V4
reg21_plp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|indyear|0|V3,data = plp5r)
summary(reg21_plp5)
reg22_plp5=felm(V1~V5+V7+I(V5*V7)+V9+V10|indyear|0|V3,data = plp5r)
summary(reg22_plp5)
reg23_plp5=felm(V1~V5+V6+I(V5*V6)+V8+V10|V3+V4|0|V3,data = plp5r)
summary(reg23_plp5)
reg24_plp5=felm(V1~V5+V7+I(V5*V7)+V9|V2+V4|0|V2,data = plp5r)
summary(reg24_plp5)


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


#solow regression
#estimating the op parameter for capital

op <- read.csv("E:/firm project/data/op.csv", header=FALSE)
attach(op)
#fixed effect
reg3=felm(log(V4)~log(V5)+log(V6)|V1+V2|0|V3,data = op)
summary(reg3)
#ols
reg3=lm(log(V4)~log(V5)+log(V6)+as.factor(V2)+as.factor(V3),data=op)

prod.ols=plm(log(V4)~log(V5)+log(V6)+as.factor(V2)+as.factor(V3),data=op,model = "pooling",index=c("V1","V2"))
summary(prod.ols)
#op estimation


op1=lm(log(V4)~log(V6)+poly(cbind(log(V5),(V7)),degree = 3)+as.factor(V2)+as.factor(V3),data=op)
summary(op1)

b1=op1$coefficients["log(V6)"]
xb1=log(as.matrix(op[,c("V6")]))%*%b1
fhat=predict(op1,op)-xb1

lag = function (x, i=op$V1 , t=op$V2) {
  if (length(i)!=length(x)||length(i)!=length(t)){
    stop (" Inputs not same length ")
  }
  x.lag = x[1:(length(x)-1)]
  x.lag[i[1:(length(i)-1)]!=i[2:length(i)]]<-NA
  x.lag[t[1:(length(i)-1)]+1!=t[2:length(i)]]<-NA
  return(c(NA,x.lag))
}

op.step2 = data.frame(lhs=(log(op$V4)-xb1),
                           k=log(op$V5),fhat=fhat,
                           k.lag=log(lag(op$V5)),
                           f.lag=lag(fhat),
                      year=op$V2,
                      ind=op$V3)
op.step2 = subset(op.step2,!apply(op.step2, 1, function(x)
  any (is.na(x))))

objective = function(betaK,degree=3){
  op2=lm(I(lhs-betaK*k) ~ poly(I(f.lag-betaK*k.lag),degree)+as.factor(year)+as.factor(ind),
            data=op.step2)
  return (sum (residuals(op2)^2))
}

fig.op <- data.frame(bk= seq( from = -1 , to =1 , by =0.05) )
fig.op$obj <- sapply(fig.op$bk , objective)
ggplot(data=fig.op,aes(x=bk,y=obj)) + geom_point()

opt.out=optim(prod.ols$coefficients["log(V5)"],
                    fn= objective, method ="Brent",lower=0,upper=1)

betaK=opt.out$par




