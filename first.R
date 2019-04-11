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
ipak(packages) #loading required packages

#Readin in the data
data<-read.table(choose.files(),sep=",",header=T ,colClasses = c("character","numeric","character","character","character"))
str(data)# Here we can observe that date variable is character
#converting date column into date column
data$Date<-as.Date(data$Date,format="%d-%m-%Y")

####################################################################################
##################### Understanding data ###########################################
####################################################################################
attach(data)# attaching data to make easy to call variables
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
ggplot(data, aes(Date, Sales.Kg.)) + geom_line() + scale_x_date('Year')  + ylab("Sales") +
  xlab("")

#Trend with leniar relation.
ggplot(data, aes(x=Date,y=Sales.Kg.)) + 
  geom_line(color = "#00AFBB") + 
  scale_x_date(date_labels="%Y-%m-%d")+stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  )

#################################################################################
######################## Preparing data for time series##########################
#################################################################################
#cleaning the data by using "tsclean".
count_ts = ts(data[, c('Sales.Kg.')],start=c(2016,1),frequency = 365)
data$clean_sales = tsclean(count_ts) # removing outliers and missing values if any!
str(count_ts)

#Plotting after cleaning the data 
ggplot() +
  geom_line(data =data, aes(x = Date, y = clean_sales)) + ylab('Sales')
#we can observe that outliers are no more in our data.


#Calculating the weekly and monthly moving average to smoother the original series
#As we do not see any patterns on the daily data, we can check from monthly and weekly moving average
data$sales_ma = ma(data$clean_sales, order=7) # Weekly moving average
data$sales_ma30 = ma(data$clean_sales, order=30)# Monthly moving average 

#plotting with both weekly and monthly moving averages.
ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_sales, colour = "Sales")) +
  geom_line(data = data, aes(x = Date, y = sales_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data, aes(x = Date, y = sales_ma30, colour = "Monthly Moving Average"))  +
  scale_colour_manual(values=c("green","blue","red"))+
  ylab('Sales')
#Here we can observe that weekly moving average seems more smoother.

#Deleting if there is any Na values
sales_ma = ts(na.omit(data$sales_ma), frequency=365)
decomp = decompose(sales_ma)#decomposing the data by using stl command to calculate the seasonal component.
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

##################################################################
################# Arima model ####################################
##################################################################

#building ARIMA model
auto.arima(sales_d1, seasonal=FALSE)
fit<-auto.arima(sales_d1, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(3,0,5) Model Residuals')

#model with appropriate p,d,q values.
fit2 = arima(sales_d1, order=c(3,0,5))
tsdisplay(residuals(fit2), lag.max=20, main='Seasonal Model Residuals')

#forecasting for future (40 days ahead)
fcast <- forecast(fit2, h=40)
plot(fcast)
summary(fit)
#to check the accuracy of our model by fixing the timewindow of 40 days.
hold <- window(ts(sales_d1), start=(nrow(data)-40))

#fitting the model with time window.
fit_no_holdout = arima(ts(sales_d1[-c(916:956)]), order=c(3,0,5))
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

###################################################################
################### Prophet model #################################
###################################################################
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




######################################################################
################# Data Preparation for ANalysis ######################
######################################################################
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

#exporting file, to put into server
write.csv(prod_sales,file="prod_sales.csv")

#checking for the outliers
prod_sales[prod_sales$sales_Kg >= (mean(prod_sales$sales_Kg)+ 3*sd(prod_sales$sales_Kg)),]
#we can see majority of high sales happening in Saturday and Friday




##############################################################################
################## Predictive model#############################################
##############################################################################
basetable<-prod_sales[,c("sales_Kg","week_weekend","day","month","IsPromo")]
str(basetable)#checking the structure of base table
#can observe character variables are not in factor format

#subsetting the factior variables
factors<-c("week_weekend","day","month","IsPromo")
#Converting into factors
basetable[,factors]<-lapply(basetable[,factors],as.factor)
str(basetable)#Again checking the structure of the basetable
#Now everything ready for the base table.

#Function to check for missing values
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  n <- length(a)
  return(c(nmiss=nmiss,len=n))
}

#applying funtion on the basetable
diag_stats<-t(data.frame(sapply(basetable, mystats)))
#we can observe that there are no missing values

#-----------------------------------------------------------------------------------------
#Splitting the data into train,val and test.
ind<- sample(x=nrow(basetable),size=nrow(basetable),replace=F)
BasetableTRAIN<-basetable[1:floor(length(ind)*0.5),]
BasetableVAL<-basetable[(floor(length(ind)*0.50)+1): floor(length(ind)*0.75),]
BasetableTEST<-basetable[(floor(length(ind)*0.75)+1): floor(length(ind)),]
BasetableTRAINbig <- rbind(BasetableTRAIN,BasetableVAL)
#plotting the distribution of our dependent variable.
ggplot(BasetableTRAIN, aes(sales_Kg)) + geom_density(fill="blue")
ggplot(BasetableTRAIN, aes(log(sales_Kg))) + geom_density(fill="blue")
ggplot(BasetableTRAIN, aes(sqrt(sales_Kg))) + geom_density(fill="blue")
#finally we can choose 
#This makes it easier to call some functions later on
yTRAIN <- BasetableTRAIN$sales_Kg
BasetableTRAIN$sales_Kg <- NULL
yVAL <- BasetableVAL$sales_Kg
BasetableVAL$sales_Kg <- NULL
yTEST <- BasetableTEST$sales_Kg
BasetableTEST$sales_Kg <- NULL
yTRAINbig <- BasetableTRAINbig$sales_Kg
BasetableTRAINbig$sales_Kg <- NULL

ggplot(BasetableTRAIN, aes(sales_Kg)) + geom_density(fill="blue")
ggplot(BasetableTRAIN, aes(log(sales_Kg))) + geom_density(fill="blue")
ggplot(BasetableTRAIN, aes(sqrt(sales_Kg))) + geom_density(fill="blue")

model<-lm(sqrt(sales_Kg)~., data=BasetableTRAIN)
summary(model)
plot(model)

pred <- predict(model, newdata = BasetableTEST)
rmse <- sqrt(sum((exp(pred) - BasetableTEST$sales_Kg)^2)/length(BasetableTEST$sales_Kg))
c(RMSE = rmse, R2=summary(model)$r.squared)

require('AUC')

#load the package randomForest
require('randomForest')
rFmodel <- randomForest(x=BasetableTRAIN,
                        y=yTRAIN,
                        xtest=BasetableVAL,
                        ytest=yVAL,
                        ntree=1000,
                        importance=TRUE)
head(plot(rFmodel))
rFmodel <- randomForest(x=BasetableTRAINbig,
                        y=yTRAINbig,
                        ntree=200,
                        importance=TRUE)
#Final performance
predrF <- predict(rFmodel,BasetableTEST)
varImpPlot(rFmodel)

oob.err=double(4)
test.err=double(4)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:4) 
  {
  rFmodel <- randomForest(x=BasetableTRAINbig,
                          y=yTRAINbig,
                          ntree=200,
                          importance=TRUE,mtry = mtry) 
  oob.err[mtry] = rFmodel$mse[200] #Error of all Trees fitted
  
  pred<-predict(rFmodel,BasetableTEST) #Predictions on Test Set for each Tree
  test.err[mtry]= with(BasetableTEST, mean( (sqrt(yTEST) - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
  }
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))



