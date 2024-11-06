
# خواندن داده ها به صورت چارچوب اطلاعاتی
choose.files()

data<-read.table(c("E:\\daneshgah\\time series\\پروزه سوم\\Iran Tele Fact..csv"),header=T ,sep = ",") 
head(data)

colnames(data)

#بردار قیمت
Price<-data[,"price"]

#بردار سری زمانی بازده
Return<-log( Price[-1] / Price[-NROW(Price)] )
Return

#فرمت تاریخ ستون تاریخ
date<-as.character(data[,"time"])
dates <- as.Date(date, "%Y%m%d")
library(zoo)

#سری زمانی قیمت
tsprice<-zoo(Price,dates) 
tspric

#سری زمانی بازده
tsreturn<-zoo(Return,dates[-1])
tsreturn

#آماره های توصیفی سری زمانی بازده

install.packages("fBasics")
library(fBasics)
basicStats(tsreturn)

#درصد سری زمانی بازده

Return1<-log( Price[-1] / Price[-NROW(Price)] )*100
Return1

#آزمون ژارک برا برای بررسی  نرمال بودن توزیع سری زمانی بازده
normalTest(return,method="jb")

#اماره آزمون چولگی
s1 <- sqrt(var(tsreturn))
t1 <- s1 / sqrt(6 / length(tsreturn))
t1

#آماره آزمون کشیدگی
s4<- var(tsreturn)^2
t4<-s4/sqrt(24/length(tsreturn))
t4

#p-value
pv=2*(1-pnorm(t4))
pv

#نمودار سری زمانی قیمت و سری زمانی بازده
plot(cbind(tsprice,tsreturn),ylab=c("Price","Return"),main="")

# خودهمبستگی
acf(Return,plot=FALSE) 
acf(Return,plot=TRUE) 

#خودهمبستگی جزیی
pacf(Return,plot=FALSE) 
pacf(Return,plot=TRUE)

#برازش مدل اتو رگرسیوبا روش mle
ord=ar(Return,method="mle")
ord

# معیار اطلاع آکاییکه
aic<- ord$aic
aic

#نمودار معیار اطلاع آکاییکه تا مرتبه 12
length(aic)
plot(c(0:12),aic,type="h",xlab="order",ylab="aic")
lines(0:12,aic,lty=2)

# برازش مدل اتورگرسیو با روش حداقل مربعات:
ord=ar(Return,method="ols")
ord

#نمودار معیار اطلاع آکاییکه تا مرتبه 12
aic<- ord$aic
aic

#برآوردگرهای ماکسیمم درستنمایی پارامترهای مدل اتورگرسیو مرتبه 3
m1=arima(return,order=c(3,0,0))
m1

#آزمون لیونگ-باکس روی باقیمانده ها برای چک کردن نیکویی برازش مدلا اتو رگرسیو مرتبه 3
Box.test(m1$residuals,lag=30,type="Ljung")
#همبستگی نگار مانده های سری زمانی بازده
acf(m1$residuals, main="Residuals of fitted AR(3) model ")

#نمودارهای تشخیصی مانده ها
tsdiag(m1, gof.lag=20)

#پیش بینی 15 گامی
install.packages("forecast")
library(forecast)
forecasts <-forecast.Arima(m1,h=15)
#نمودار پیش بینی خارج نمونه ای
plot.forecast(forecasts)


