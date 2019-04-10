# time series
# dataset : Monthly milk production: pounds 
# per cow. Jan 62 - Dec 75

library(forecast)
library(tseries)
library(fpp)


ml <- read.csv("prod_sales.csv",stringsAsFactors = F)

ml$sales_Kg<-ifelse(ml$sales_Kg == 0,0.001,ml$sales_Kg)
ml <- data.frame(ml[,c("sales_Kg")])
ml$ml...c..sales_Kg...<-log(ml$ml...c..sales_Kg...)

sum(is.na(ml))
str(ml)

ml <- ts(ml,start=c(2016,1),frequency = 365)
plot.ts(ml)
dec = decompose(ml)
plot(dec)


# subsetting data 

mlTr <- window(ml, start=c(2016,1),end=c(2018,192))
              
mlTst <- window(ml,start=c(2018,193),
                end=c(2018,232))


str(dec)

# components of time series
dec$seasonal 
dec$trend
dec$random
dec$type # Type of seasonality


decM <- decompose(mlTr, type = "multiplicative")
decM$x # original series
decM$type
decM$seasonal
decM$trend
decM$random
# ts = trend * seasonal * random 
plot(decM)


# Exponential smoothing
es = ses(mlTr,h=40) # h - number of periods for forecasting
plot(es)
accuracy(es,x=window(ml,start=c(2018,193),
                     end=c(2018,232)))

checkresiduals(es)

Box.test(es$residuals,lag = 20, type = "Ljung-Box")

# fitting holt's model
hol <- holt(mlTr,h=40)
plot(hol)
accuracy(hol,x=window(ml,start=c(2018,193),
                      end=c(2018,232)))

checkresiduals(hol)
Box.test(hol$residuals,lag = 20, type = "Ljung-Box")

# holts winter model
hw <- hw(mlTr,h=40,seasonal = "additive")
plot(hw)
accuracy(hw,x=window(ml,start=c(2018,193),
                     end=c(2018,232)))

checkresiduals(hw)
Box.test(hw$residuals,lag = 20, type = "Ljung-Box")



# automatic model
auto <- stlf(mlTr)
summary(auto)

plot(auto)
foc <- forecast(auto, h=40)
plot(foc)
accuracy(foc,x=window(ml,start=c(2018,193),
                      end=c(2018,232)))

Box.test(auto$residuals,type = "Ljung-Box")


















