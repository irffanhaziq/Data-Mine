# mining Time Series Data

set.seed(123)
data1 = rnorm(12)

#create time series data as qurtely data starting 
#from 2nd quarter of 2022 frequency 4

X =ts(data1, frequency = 4,start = c(2022,2))

#create monthly time series data starting frequency 12
#form jan 2021

X2 = ts(data1,frequency = 12,start=c(2021,1))
X2


#R adopting\ iso 8601
data = Sys.Date()
time = Sys.time()

date2  = as.Date("2023-5-12")
class(date2)


#create date starting from 2016-01-01 on to 2018-12-31
daily_index = seq.Date(from=as.Date("2016-01-01"),to=as.Date("2018-12-31"), by="day")
#3days inteval
day3_index= seq.Date(from=as.Date("2016-01-01"),to=as.Date("2018-12-31"), by="3 days")

#data dates.formate2.csv
data_df=read.csv(file.choose(),stringsAsFactors = F,
                 sep=";")

#transform char to date format
attach(data_df)
names(data_df)
US_format_New=as.Date(US_format, format="%m/%d/%Y")

Japnese_format_new=(as.Date(Japanese_format,format="%m/%d/%Y"))


#Excel Numeric Format
#in excell, the origin of date is December 30, 1899
#In R, the origin January 1, 1970 


Excel_f<- as.Date(Excel_Numeric_Format, origin=as.Date("1899-12-30"))


#class times series data in R
#POSIX class

time_str<- "2017-01-20 23:59:34" 
time_str_ctl<-as.POSIXct(time_str)

#zoo class
install.packages("TSstudio")
library(TSstudio)
data(US_indicators)
library(zoo)
Vehicle_Salesl<- zoo(x=US_indicators$'Vehicle Sales', frequency=12)

#ts class
data(USgas)
class(USgas)

#xts class
library(xts)
Vehicle_Sales2 = xts(x=US_indicators$'Vehicle Sales', frequency=12,
                     order.by=US_indicators$Date)

###time series decop
data(USgas)
#Seasonal componant
ts.plot(USgas, main="US Monthly Natural Gas Consumption",
        ylab="Billon Cubic Feet",xlab="year")

ts_heatmap(USgas)

#cycle companr
data("USUnRate")
ts.plot(USUnRate, main="US Monthly Unemployment Rate",
        ylab="Unemployment Rate",xlab="year")
ts_heatmap(USUnRate)

#types of decompostion 
#additive structure
ts.plot(USgas, main="US Monthly Natural Gas Consumption",
        ylab="Billon Cubic Feet",xlab="year")

usg_decompose = decompose(USgas)
plot(usg_decompose)

#extract each componant

names(usg_decompose)
#seasonal componant
usg_decompose$seasonal
#seasonal trend
usg_decompose$trend
#seasonal random
usg_decompose$random


#multiplicatiave structure
data("AirPassengers")
ts.plot(AirPassengers, main="Monthly Airline Passenger",
        ylab="Thousand",xlab="year")
air_decompose = decompose(AirPassengers, type="multiplicative")

names(air_decompose)
#seasonal componant
air_decompose$seasonal
#seasonal trend
air_decompose$trend
#seasonal random
air_decompose$random

#convert mulitplicative structure to additve
library(forecast)


#mulitplicative
data("AirPassengers")
AirP=BoxCox.lambda(AirPassengers)
AirPT=BoxCox(AirPassengers, lambda = AirP)

par(mfrow=c(1,2))
plot.ts(AirPassengers)
plot.ts(AirPT)

#data Towel.csv

data = read.csv(file.choose(),header = T)
yt =ts(data)
par(mforw=c(2,1))
plot.ts(yt,main="Paper twoel daily sale",ylab='yt')

#data is not stationary
#do a differentation
zt1 =diff(yt,differences=1)
plot.ts(zt1,main="1st difference data", ylab="zt")

#arima model identification

acf(zt1,main="sampple Autocorrelaction Function")
pacf(zt1,main="sampple Partial Autocorrelaction Function")

#based on ACF and PACF plot
#the model should be arima(0,1,1)

#fir arima(0,1,1) to dataset
model = arima(yt,order=c(0,1,1))
summary(model)

#plot model to observed dataset
plot.ts(yt,main="Paper twoel daily sale",ylab='yt')
lines(fitted(model),col="red",lty=2)

legend("bottomleft",
       c("Observed Data","Model ARIMA(0,1,1)"),
       col=c(1,2),lty = c(1,2))



##Diganostic check 
#residual Analysis

f.value= forecast(model, h=5)
residual = f.value$residuals

#1. residual should not be autocorelated
#check using acf
acf(residual,lag.max = 20)

#Based on acf plot, residual is not autocoorelated

#2. Residual should approximate normal dis
hist(residual)

#the residua; should having constant variance over time
plot.ts(residual)

#since all assumptions is fulfied, the arima(0,1,1)
#model can be used for predication

#forecasting from arima(0,1,1)model
plot.ts(yt,main= "paper towel",ylab="yt")
lines(fitted(model),col="red",lty=2)

forecasting = predict(model, n.ahead = 5)
#50% confidence interval
U = forecasting$pred +0.69*forecasting$se
L = forecasting$pred -0.69*forecasting$se

#forecasting plot 
ts.plot(yt,forecasting$pred,U,L,col=c(1,2,4,4),
        lty=c(1,1,2,2))
legend("bottomleft", c("observed data","forecast","confidence interval"),col=c(1,2,4),lty=c(1,1,2))




##Time Series Clustering
sample2
ts.plot(sample2)
par(mfrow=c(3,3))
plot.ts(sample2[,1]);plot.ts(sample2[,2]);plot.ts(sample2[,3]);
plot.ts(sample2[,4]);plot.ts(sample2[,5]);plot.ts(sample2[,6]);
plot.ts(sample2[,7]);plot.ts(sample2[,8]);plot.ts(sample2[,9])
dev.off()
##determine the cluster 
install.packages("dtw")
library(dtw)

#put a label on each time series data
D.Labels = rep(1:60)


#compute distance matrix
distMatrix = dist(sample2, method="DTW")

#plotthe cluster
TSCluster = hclust(distMatrix, method="average")
plot(TSCluster,label=D.Labels,
     main="time series clustering")

#get 6 cluster
rect.hclust(TSCluster, k=6)

#justify your results
#cluster 1
windows(10,10)
par(mfrow=c(2,4))
plot.ts(sample2[,34]); plot.ts(sample2[,35]);plot.ts(sample2[,32])
plot.ts(sample2[,37]); plot.ts(sample2[,33]);plot.ts(sample2[,31])
plot.ts(sample2[,36])

#Cluster 2
windows(10,10)
par(mfrow=c(1,3))
plot.ts(sample2[,18]); plot.ts(sample2[,16]);plot.ts(sample2[,20])


Data.Ks
dim(Data.Ks)
names(Data.Ks)

#Times series Classification
library(party)
classId= rep(as.character(1:6),each=10)
Class.Model = ctree(classId~.,data=Data.Ks) #consider all colume
plot(Class.Model)

#measure precison of prediction model 
pClassId= predict(Class.Model)
table(classId,pClassId)

#prediction accuracy
sum(classId==pClassId)/nrow(Data.Ks)
