#Import dataset from the following link: AirQuality Data Set
#Perform the following written operations:
#Question 1. Read the file in Zip format and get it into R.
airquality <- read.csv("D:/data/AirQuality.csv")

#Question 2. Create Univariate for all the columns.
library(ggplot2)
airquality %>%   keep(is.numeric) %>%  gather() %>%  ggplot(aes(value)) +  facet_wrap(~ key,scales = "free") +   geom_histogram()

# Also Ploting can be done individually for each variable

hist(airquality$Ozone ,xlab = "ozone", ylab = "Frequency",main="Histogram of ozone",col="purple")
hist(airquality$Solar.R ,xlab = "solar.r", ylab = "Frequency",main="Histogram of solar.r",col="darkblue")
hist(airquality$Wind ,xlab = "wind", ylab = "Frequency",main="Histogram of wind",col="green")
hist(airquality$Temp ,xlab = "temp", ylab = "Frequency",main="Histogram of temp",col="pink")
hist(airquality$Month ,xlab = "month", ylab = "Frequency",main="Histogram of month",col="blue")
hist(airquality$Day ,xlab = "day", ylab = "Frequency",main="Histogram of day",col="red")

#Question 3.Check for missing values in all columns.

summary(airquality)

#Question 4.Impute the missing values using appropriate methods.

str(airquality)
newair =airquality
dim(newair)
str(newair)
summary(newair)
hist(newair$Solar.R ,xlab = "Solar.R", ylab = "frequency",main="histogram of Solar.R",col="red")
mean(newair$Solar.R)
mean(newair$Solar.R,na.rm = T)
newair$Solar.R[is.na(newair$Solar.R)]<- mean(newair$Solar.R,na.rm = T)
summary(newair)
newair$Solar.R
hist(newair$Solar.R ,xlab = "Solar.R", ylab = "frequency",main="histogram of Solar.R",col="red")

#Question 5. Create bi-variate analysis for all relationships.

library(psych)
pairs.panels( airquality[,c(1,2,3,4,5,6)],method = "pearson", # correlation methodhist.col = "red",density = TRUE,ellipses = TRUE,lm=TRUE,main ="Bivariate Scatter plots with Pearson Correlation & Histogram")
              
#Question 6. Test relevant hypothesis for valid relations.
str(airquality)
t.test(x=airquality$Ozone, y=airquality$Solar.R ,alternative = "two.sided",mu=0 ,paired = TRUE)

t.test(x=airquality$Temp, y=airquality$Wind ,alternative = "two.sided",mu=0 ,paired = TRUE)

#Question 7. Create cross tabulations with derived variables.
attach(airquality)
unique(Wind)
unique(Temp)
x<- cut(Wind,quantile(Wind))
x<- cut(Wind,breaks = seq(1,21,3),labels = c("wind1","wind2","wind3","wind4","wind5","wind6"))
y<- cut(Temp,quantile(Temp))
y<- cut(Temp,breaks = seq(55,100,9),labels = c("temp1","temp2","temp3","temp4","temp5"))
table(x,y)

mytable<- xtabs(~x+y,data = airquality)
mytable
#crosstabulate
library(gmodels)
CrossTable(x,y)

#Question 8. Check for trends and patterns in time series.
#THIS TOPIC NOT COVERED
  
  
  
#Question 9. Find out the most polluted time of the day and the name of the chemical compound.
#THIS TOPIC NOT COVERED