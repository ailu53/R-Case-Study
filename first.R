#cleaning the Environment
rm(list = ls())
#setting thee working directory
setwd(choose.dir())

# load and install packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#packages <- c("httr", "twitteR", "SnowballC", "tm", "statnet", "igraph")
packages <- c("sqldf","readxl","ggplot2", "dplyr","forecast","tseries")
ipak(packages)

#Readin in the data
data<-read.table(choose.files(),sep=",",header=T ,colClasses = c("character","numeric","character","character","character"))
str(data)# Here we can observe that date variable is character

#converting date column into date column
data$Date<-as.Date(data$ï..Date,format="%d-%m-%Y")
data$ï..Date<-NULL
attach(data)# attaching data to make easy to call variables
require(ggplot2)
#Exploratory data analysis
#checking the distribution of Store_ID
ggplot(data = data) +
  geom_bar(mapping = aes(x = Store_ID)) # Here we can observe that there is only one store.

#checking the distribution of Article_ID
ggplot(data = data) +
  geom_bar(mapping = aes(x = Article_ID))# Also observed that there is only one Article_ID.
#As we have only one Article_ID and Store_ID so better to drop both.


ggplot(data = data) +
  geom_bar(mapping = aes(x = IsPromo,colour=IsPromo),color = "#00AFBB") # we can observe that they are not giving much promotions.

#Plotting the sales trend
ggplot(data, aes(Date, Sales.Kg.)) + geom_line() + scale_x_date('month')  + ylab("Sales") +
  xlab("")

#Trend with leniar relation.
ggplot(data, aes(x=Date,y=Sales.Kg.)) + 
  geom_line(color = "#00AFBB") + 
  scale_x_date(date_labels="%Y-%m-%d")+stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  )

#cleaning the data by using "tsclean".
count_ts = ts(data[, c('Sales.Kg.')])
data$clean_sales = tsclean(count_ts)

#Plotting after cleaning the data 
ggplot() +
  geom_line(data =data, aes(x = Date, y = clean_sales)) + ylab('Sales')
#we can observe that outliers are no more in our data.

#Calculating the weekly and monthly moving average to smoother the original series
data$sales_ma = ma(data$clean_sales, order=7) # using the clean sales with no outliers
data$sales_ma30 = ma(data$clean_sales, order=30)

#plotting with both weekly and monthly moving averages.
ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_sales, colour = "counts")) +
  geom_line(data = data, aes(x = Date, y = sales_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data, aes(x = Date, y = sales_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')
#Here we can observe that weekly moving average seems more smoother.

#Deleting if there is any Na values
sales_ma = ts(na.omit(data$sales_ma), frequency=30)
decomp = stl(sales_ma, s.window="periodic")#decomposing the data by using stl command to calculate the seasonal component.
deseasonal_sales <- seasadj(decomp)
plot(decomp)

#Statistical test to check for stationary
adf.test(sales_ma, alternative = "stationary")
#still we have to deseasonal as per the results

#plotting ACF and PACF
Acf(sales_ma, main='')
Pacf(sales_ma, main='')
#still we have to deseasonal as per the results

#Further differencing to deseasonal bu 1
sales_d1 = diff(deseasonal_sales, differences = 1)
plot(sales_d1)
adf.test(sales_d1, alternative = "stationary")
#plottng ACF and PACF
Acf(sales_d1, main='')
Pacf(sales_d1, main='')
#

#building ARIMA model
auto.arima(deseasonal_sales, seasonal=FALSE)
fit<-auto.arima(deseasonal_sales, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,4) Model Residuals')

#model with appropriate p,d,q values.
fit2 = arima(deseasonal_sales, order=c(1,1,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#forecasting for future (40 days ahead)
fcast <- forecast(fit2, h=40)
plot(fcast)

#to check the accuracy of our model by fixing the timewindow of 40 days.
hold <- window(ts(deseasonal_sales), start=(nrow(data)-40))

#fitting the model with time window.
fit_no_holdout = arima(ts(deseasonal_sales[-c(922:962)]), order=c(1,1,7))

#forecasting 
fcast_no_holdout <- forecast(fit_no_holdout,h=40)
plot(fcast_no_holdout, main=" ")#predicted values
lines(ts(deseasonal_sales))#ploting actual values

#fitting ARIMA model with seasonality.
fit_w_seasonality = auto.arima(deseasonal_sales, seasonal=TRUE)
summary(fit_w_seasonality)

#Forecasting with seasonality
seas_fcast <- forecast(fit_w_seasonality, h=40)
plot(seas_fcast)
summary(seas_fcast)
