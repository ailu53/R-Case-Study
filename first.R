#cleaning the Environment
rm(list = ls())
#setting thee working directory
setwd(choose.dir())

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

#
ggplot(data, aes(x=Date,y=`Sales(Kg)`)) + 
  geom_line(color = "#00AFBB") + 
  scale_x_date(date_labels="%Y-%m-%d")+stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  )

