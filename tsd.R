kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) #read the time series data into R
kings
kingstimeseries <- ts(kings)  #store the data in a time series object in R
kingstimeseries
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") #number of births per month in New York city, from January 1946 to December 1959
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") #monthly sales for a souvenir shop at a beach resort town in Queensland, Australia, for January 1987-December 1993
ssouvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
ssouvenirtimeseries
plot.ts(kingstimeseries) #plot the time series of the age of death of 42 successive kings of England
plot.ts(birthstimeseries) #Time series of the number of births per month in New York city
plot.ts(ssouvenirtimeseries) #plot the time series of the monthly sales for the souvenir shop at a beach resort town in Queensland
#transformed time series that can be described using an additive model. 
#For example, we can transform the time series by calculating the natural log of the original data:
logsouvenirtimeseries <- log(ssouvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#Decomposing Time Series
library("TTR")
#To smooth the time series using a simple moving average of order 3, and plot the smoothed time series data
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)


#Decomposing Seasonal Data
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal # get the estimated values of the seasonal component
plot(birthstimeseriescomponents)

#to seasonally adjust the time series of the number of births per month in New York city, we can estimate the seasonal component using “decompose()”
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

#SIMPLE EXPONENTIAL SMOOTHING
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)

rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
rainseriesforecasts$fitted
plot(rainseriesforecasts)

rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
library("forecast")
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)

acf(rainseriesforecasts2$residuals, lag.max=20) #to calculate a correlogram of the in-sample forecast errors for the London rainfall data for lags 1-20
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(rainseriesforecasts2$residuals)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(rainseriesforecasts2$residuals)


#Holt’s Exponential Smoothing
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.forecast(skirtsseriesforecasts2)
acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(skirtsseriesforecasts2$residuals)
plotForecastErrors(skirtsseriesforecasts2$residuals)


logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(souvenirtimeseriesforecasts2$residuals)
plotForecastErrors(souvenirtimeseriesforecasts2$residuals)


#Differencing a Time Series arima models
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)


#Selecting a Candidate ARIMA Model 
#Example of the Ages at Death of the Kings of England
acf(kingtimeseriesdiff1, lag.max=20)
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE)
pacf(kingtimeseriesdiff1, lag.max=20)
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE)


#Example of the Volcanic Dust Veil in the Northern Hemisphere
#Let’s take another example of selecting an appropriate ARIMA model.

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)
acf(volcanodustseries, lag.max=20, plot=FALSE)
pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

# Forecasting Using an ARIMA Model   
#Example of the Ages at Death of the Kings of England
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima
library("forecast") # load the "forecast" R library
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot.forecast(kingstimeseriesforecasts)
acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(kingstimeseriesforecasts$residuals)
plotForecastErrors(kingstimeseriesforecasts$residuals)
volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima


volcanodustseriesforecasts <- forecast.Arima(volcanodustseriesarima, h=31)
volcanodustseriesforecasts
plot.forecast(volcanodustseriesforecasts)
acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)
plotForecastErrors(volcanodustseriesforecasts$residuals)
mean(volcanodustseriesforecasts$residuals)

