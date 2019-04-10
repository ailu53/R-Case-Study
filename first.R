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
data$Date<-as.Date(data$Date,format="%d-%m-%Y")

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
count_ts = ts(data[, c('Sales.Kg.')],start=c(2016,1),frequency = 365)
data$clean_sales = tsclean(count_ts)
str(count_ts)
#Plotting after cleaning the data 
ggplot() +
  geom_line(data =data, aes(x = Date, y = clean_sales)) + ylab('Sales')
#we can observe that outliers are no more in our data.
cycle(count_ts)
#Calculating the weekly and monthly moving average to smoother the original series
data$sales_ma = ma(data$clean_sales, order=7) # using the clean sales with no outliers
data$sales_ma30 = ma(data$clean_sales, order=30)

#plotting with both weekly and monthly moving averages.
ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_sales, colour = "Sales")) +
  geom_line(data = data, aes(x = Date, y = sales_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data, aes(x = Date, y = sales_ma30, colour = "Monthly Moving Average"))  +
  ylab('Sales')
#Here we can observe that weekly moving average seems more smoother.

#Deleting if there is any Na values
sales_ma = ts(na.omit(data$sales_ma), frequency=365)
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
auto.arima(sales_d1, seasonal=FALSE)
fit<-auto.arima(sales_d1, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,0,4) Model Residuals')

#model with appropriate p,d,q values.
fit2 = arima(sales_d1, order=c(3,0,5))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#forecasting for future (40 days ahead)
fcast <- forecast(fit2, h=40)
plot(fcast)
summary(fit)
#to check the accuracy of our model by fixing the timewindow of 40 days.
hold <- window(ts(sales_d1), start=(nrow(data)-40))

#fitting the model with time window.
fit_no_holdout = arima(ts(sales_d1[-c(922:962)]), order=c(1,1,7))

#forecasting 
fcast_no_holdout <- forecast(fit_no_holdout,h=40)
plot(fcast_no_holdout, main=" ")#predicted values
lines(ts(sales_d1))#ploting actual values

#fitting ARIMA model with seasonality.
fit_w_seasonality = auto.arima(deseasonal_sales, seasonal=TRUE)
summary(fit_w_seasonality)

#Forecasting with seasonality
seas_fcast <- forecast(fit_w_seasonality, h=40)
plot(seas_fcast)
summary(seas_fcast)
#residuals plotting
hist(residuals(fit_w_seasonality))

#creating requre variables for data visualization
require(lubridate)# package to work with dates
data$month<-months(data$Date)#extracting month
data$day<-weekdays(data$Date)#extracting days
data$week_weekend<- ifelse(data$day %in% c("Sunday","Saturday"),"Weekend","Weekday")#separating weekday and weekend.
data$year<-year(data$Date)#extracting year

data <- data %>% #renaming the sales variable
  rename(sales_Kg=Sales.Kg.)

#creating new data frame with reqired columns for further visualization.
prod_sales<-data[,c("Date","IsPromo","sales_Kg","month","day","week_weekend","year")]

write.csv(prod_sales,file="prod_sales.csv")
getwd()


#Modeling usind Prophet library (Facebook provided)
#plotting to check the distribution of data
qplot(Date,Sales.Kg.,data=data)# observed that the data is scatterd

#Transforming the data by "log"
ds<-data$Date
data$Sales.Kg.<-ifelse(data$Sales.Kg. == 0,0.001,data$Sales.Kg.)
y<-log(data$Sales.Kg.)
df=data.frame(ds,y)
qplot(ds,y,data=df)#can see some patterns though some sctter takes place

#loading Prophet package
library(prophet)
#modeling
m<-prophet(df,daily.seasonality = T)
future<-make_future_dataframe(m,40)

#Forecasting
forecast<-predict(m,future)
pred<-exp(forecast$yhat)

#plotting the forecasted values and actual
plot(m,forecast)

#plotting to check for the trends (daily, weekly and yearly)
prophet_plot_components(m,forecast)

