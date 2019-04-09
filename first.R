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

count_ts = ts(data[, c('Sales.Kg.')])
data$clean_sales = tsclean(count_ts)

ggplot() +
  geom_line(data =data, aes(x = Date, y = clean_sales)) + ylab('Sales')


data$sales_ma = ma(data$clean_sales, order=7) # using the clean count with no outliers
data$sales_ma30 = ma(data$clean_sales, order=30)

ggplot() +
  geom_line(data = data, aes(x = Date, y = clean_sales, colour = "counts")) +
  geom_line(data = data, aes(x = Date, y = sales_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = data, aes(x = Date, y = sales_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


sales_ma = ts(na.omit(data$sales_ma), frequency=30)
decomp = stl(sales_ma, s.window="periodic")
deseasonal_sales <- seasadj(decomp)
plot(decomp)

adf.test(sales_ma, alternative = "stationary")


Acf(sales_ma, main='')

Pacf(sales_ma, main='')

sales_d1 = diff(deseasonal_sales, differences = 1)
plot(sales_d1)
adf.test(sales_d1, alternative = "stationary")

Acf(sales_d1, main='')

Pacf(sales_d1, main='')


