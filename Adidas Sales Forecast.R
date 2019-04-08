#Loading the dataset
library(fpp)
library(fpp2)
#reading data from csv file
data = read.csv("C:/Users/Yash/Downloads/adidas_revenue1.csv")
data

#converting into time series data
data<-ts(data, start=c(2000,1), end=c(2017,1), frequency = 4)
data

#extracting sales data
y=data[,2]
y

#Set training dataset and testing dataset
train_data = window(y,start=c(2000,1), end=c(2013,4))
test_data = window(y,start=2014)
train_data
test_data

#plot raw dataset
plot(y,main="Adidas Quarterly Sales", lwd=3)
plot(y,main="Adidas Revenue", xlab="year", ylab="$millions", lwd=3)
lines(ma(y,9),col="orange",lwd=4)

#seasonlity plot
seasonplot(y,main="Seasonal plot: Adidas sales",
           year.labels = TRUE, year.labels.left = TRUE,
           col=1:20, pch=19,lwd=2)

monthplot(y, main="Seasonal plot: Adidas sales", 
          xlab="quarter", ylab = "$million")

#Autocorrelation
Acf(y, lwd=5, main="Adidas Quarterly Sales")

#Looking at the raw dataset, ADIDAS sales have a strong seasonal and increasing trend 
#pattern. Out of four quarters, the third quarter generally has better performance.
#It also has a strong correlation with its lagged data.

#SIMPLE EXPONENTIAL SMOOTHING
AQS <- window(y, start=2000)
autoplot(AQS) + ylab("Sales ($millions)") + xlab("Year")
#PLOT ADIDAS QUATERLY SALES FROM 2000 to 2017 
AQS<-window(y,start=2000,end=2017)
plot(AQS, ylab="Sales ($millions)",xlab="Year")
fit1 <-ses(AQS, alpha=0.2, initial="simple", h=3)
fit2 <-ses(AQS, alpha=0.6, initial="simple", h=3)
fit3 <-ses(AQS, alpha=0.89, initial="simple", h=3)

plot(fit1, plot.conf=FALSE, main="Adidas Quaterly sales from 2000 to 2017", ylab="Sales ($millions)", xlab="Year", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.89)),pch=1)

#HOLT LINEAR MODEL
AQS <- window(y, start=2000)    
fc <- holt(AQS, h=5)              

#HOLT'S LINEAR TREND METHOD

fc <- holt(AQS, h=15)
autoplot(AQS) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Sales ($millions)") +
  guides(colour=guide_legend(title="Forecast"))

#HOLT-WINTERS SEASONAL METHOD:MULTIPLICATIVE AND ADDITIVE 
AQSS <- window(y,start=2000)
fit1 <- hw(AQSS,seasonal="additive")
fit2 <- hw(AQSS,seasonal="multiplicative")
autoplot(AQSS) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Sales ($millions)") +
  ggtitle("Adidas Quarterly Sales") +
  guides(colour=guide_legend(title="Forecast"))
