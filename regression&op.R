library(lfe)
library(sandwich)
library(lmtest)
library(plm)
library(ggplot2)
library(pracma)


dat1 <- read.csv("E:/firm project/data/dat1.csv", header=FALSE)

attach(dat1)

indyear=dat1$V3*10000+dat1$V4
reg1=felm(V1~V5+I(V5*V6)|indyear+V2|0|V3,data = dat1)
summary(reg1)

dat2 <- read.csv("E:/firm project/data/dat2.csv", header=FALSE)
attach(dat2)
indyear=dat2$V3*10000+dat2$V4
reg2=felm(V1~V5+V6+I(V5*V6)+V7|indyear|0|V3,data = dat2)
summary(reg2)

#solow regression

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




