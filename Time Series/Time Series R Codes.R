## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, cache=TRUE, warning=FALSE, message=FALSE)
library(tidyverse)
library(gridExtra)
library(fpp2)
library(sandwich)
library(lmtest)
library(xts)

##------------------------------------------------------------------------------
#Set Working Directory
setwd("C:/Users/ReggyRyt/OneDrive - ualberta.ca/ECON 509 Time Series/Paper/Italy Unemployment")

##------------------------------------------------------------------------------
#Import Data
YUR_Data <- read.csv("YUR Time Series.csv", sep=",",  header=TRUE)
#YUR_Data$Date <- seq(from = as.Date("2004-01-01"), to = as.Date("2020-09-30"), by = 'month')
GT_Data <- read.csv("multiTimeline (2).csv", sep=",",  header=TRUE, skip = 2)
names(GT_Data)[2] <- "GT"

##------------------------------------------------------------------------------
# load Italy recession dates
Italy <-read.table("ITRES.csv", sep=",", header=TRUE)
ItRec <- ts(Italy[2], start=2004, frequency=12)

# find peaks and troughs
tmp0 <- diff(ItRec)
peak <- time(tmp0)[which(tmp0=="1")-1]
trough <- time(tmp0)[which(tmp0=="-1")]
# set as data frame
recessions.df <- data.frame(Peak=peak,Trough=trough)

##------------------------------------------------------------------------------
##Preparing And Plotting Data
df.YUR <- ts(data.frame(YUR_Data),start=c(2004, 1), frequency=12)
df.GT <- ts(data.frame(GT_Data),start=c(2004, 1), end=c(2020, 9), frequency=12)

Data <- ts(cbind(df.YUR[,2], df.GT[,2]), start=c(2004, 1), frequency=12)
colnames(Data) <- c("YUR","GT")

p1 <- autoplot(Data[,2]) + xlab("Year") + ylab("GT") + theme_bw() 
p2 <- autoplot(Data[,1]) + xlab("Year") + ylab("YUR") + theme_bw() 
grid.arrange(p1,p2,nrow=2)

autoplot(as.xts(Data[,1])) + xlab("Month") + ylab("YUR") + theme_bw() +
  ggtitle("YUR Time Series") +
  geom_rect(data=recessions.df,aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill='grey', alpha=0.4)

autoplot(as.xts(Data[,2])) + xlab("Month") + ylab("GT") + theme_bw() +
  ggtitle("GT Time Series") +
  geom_rect(data=recessions.df,aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill='grey', alpha=0.4)

##------------------------------------------------------------------------------
#TRAMO-SEATS Seasonal Adjustment
library(RJDemetra)
TS.YUR <- tramoseats(Data[,1])
TS.YUR.SA <- ts(data.frame(TS.YUR$final$series),start=c(2004, 1),frequency=12)

TS.GT <- tramoseats(Data[,2])
TS.GT.SA <- ts(data.frame(TS.GT$final$series),start=c(2004, 1), frequency=12)

df.TS <- ts(cbind(TS.YUR.SA[,"sa"],TS.GT.SA[,"sa"]),
                  start=c(2004, 1), frequency=12)
#Date <- seq(from = as.Date("2004-01-01"), to = as.Date("2020-09-30"), by = 'month')
#df.TS1 <- cbind.data.frame(Date,df.TS)
colnames(df.TS) <- c("YUR","GT")

autoplot(df.TS) + xlab("Year") + ylab("") + theme_bw() +
  ggtitle("Seasonaly Adjusted Data")

p3 <- autoplot(df.TS[,1]) + xlab("Year") + ylab("YUR") + theme_bw() 
p4 <- autoplot(df.TS[,2]) + xlab("Year") + ylab("GT") + theme_bw()
grid.arrange(p3,p4, nrow = 2)

autoplot(as.xts(df.TS[,1])) + xlab("Month") + ylab("YUR") + theme_bw() +
  ggtitle("Seasonaly Adjusted YUR Time Series") +
  geom_rect(data=recessions.df,aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill='grey', alpha=0.5)

autoplot(as.xts(df.TS[,2])) + xlab("Month") + ylab("GT") + theme_bw() +
  ggtitle("Seasonaly Adjusted GT Time Series") +
  geom_rect(data=recessions.df,aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf),
            fill='grey', alpha=0.5)

##------------------------------------------------------------------------------
library("ggpubr")
#Correlation
cor.test(Data[,1],Data[,2])
cor.test(df.TS[,1],df.TS[,2])

##------------------------------------------------------------------------------
#Summary Statistics
summary(df.TS)
summary(Data)

##------------------------------------------------------------------------------
## Replication
df_replication <- window(df.TS, end=c(2015, 3))
autoplot(df_replication) + xlab("Year") + ylab("") + theme_bw() +
  ggtitle("Plot of Replicated Time Series")

##Unit Root Test
source("urtests.r")
testYUR <- ur.test(diff(df_replication[,1]), trend="c", method="adf.ols", penalty="MAIC", kmax=10)
print.ur.test(testYUR)

testGT <- ur.test(diff(df_replication[,2]), trend="c", method="adf.ols", penalty="MAIC", kmax=10)
print.ur.test(testGT)

##Johansen Test
library(urca)
library(vars)
lag <- VARselect(df_replication, type="const", lag.max=10 )
laglenght <- data.frame(lag$criteria)
colnames(laglenght) <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10")
write.csv(laglenght,"laglenght.csv")

jotest=ca.jo(df_replication, type="trace", K=8, ecdet="const", spec="longrun")
summary(jotest)

fitvar1 <- VAR(diff(df_replication), p=8, type="none")
var <- restrict(fitvar1, method='ser', thresh = 2)
summary(var)

matrix <- matrix(c(1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,
                   0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1), 
                 nrow=2 ,ncol=16, byrow=TRUE)
var1 <- restrict(fitvar1, method="man", resmat = matrix )
summary(var1)

##------------------------------------------------------------------------------
##New Data

##Unit Root Test
testYUR_ <- ur.test(df.TS[,1], trend="c", method="adf.ols", penalty="MAIC", kmax=10)
print.ur.test(testYUR_)

testGT_ <- ur.test(df.TS[,2], trend="c", method="adf.ols", penalty="MAIC", kmax=10)
print.ur.test(testGT_)

##Johansen Test
lag_ <- VARselect(df.TS, type="const", lag.max=10 )
lag_
laglenght_ <- data.frame(lag_$criteria)
colnames(laglenght_) <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10")
write.csv(laglenght_,"laglenght_.csv")

jotest_=ca.jo(df.TS, type="trace", K=4, ecdet="const", spec="transitory")
summary(jotest_)

fitvar_ <- VAR(diff(df.TS), p=4, type="none")
var_ <- restrict(fitvar_, method='ser', thresh = 2)
summary(var_)

##------------------------------------------------------------------------------
##ARIMAX
p1 <- ggAcf(df.TS[,1]) + ggtitle("")
p2 <- ggPacf(df.TS[,1]) + ggtitle("")
grid.arrange(p1,p2,ncol=2)

p3 <- ggAcf(diff(df.TS[,1])) + ggtitle("") + theme_bw()
p4 <- ggPacf(diff(df.TS[,1])) + ggtitle("") + theme_bw()
grid.arrange(p3,p4,nrow=2)

ARIMAX <- Arima(df.TS[,1][-1], order=c(0,1,2), xreg = diff(df.TS[,1]))
summary(ARIMAX)
checkresiduals(ARIMAX, lag=10)
##------------------------------------------------------------------------------
## Backtesting for ARIMAX and VAR
# train:test = 80:20
#end of training set 2017-05
n.end <- 2017+4/12

#test set: 2017-06 - 2020-09
#40 observations in test set
predVAR <- matrix(rep(NA,40*4),40,4)
colnames(predVAR) <- c("Actual","YUR.l1","Forecast","Level")

predARIMAX <- matrix(rep(NA,40*2),40,2)
colnames(predARIMAX) <- c("Actual","Forecast")

##Backtesting for VAR
#Parameter Restriction
mat <- matrix(c(1,1,1,1,0,0,0,0,
                1,0,1,0,1,0,1,0), 
              nrow=2 ,ncol=8, byrow=TRUE)

#loop for rolling estimation
for(i in 1:40){
  start <- 2004+(i-1)*1/12
  end <- n.end+(i-1)*1/12
  train_set <- window(df.TS, start, end)
  predVAR[i,"Actual"] <- window(df.TS[,'YUR'], end+1/12, end+1/12)
  predVAR[i,"Forecast"] <-forecast(
    restrict(VAR(diff(train_set), p=4, type="none"),
             method="man", resmat = mat),h=1)$forecast$YUR$mean
  #Conditional forecast
  predVAR[i,"YUR.l1"] <- window(df.TS[,'YUR'], end, end)
  predVAR[i,"Level"] <- predVAR[i,"YUR.l1"] + predVAR[i,"Forecast"]
}

#Unconditional forecast
# for(i in 1:18){ 
#   for(j in 2:18){
#     predVAR[,"YUR.l1"][1] <- window(df.TS[,'YUR'], start=c(2019,2), end=c(2019,2))
#     predVAR[,"Level"][i] <- predVAR[,"YUR.l1"][i] + predVAR[,"Forecast"][i]
#     predVAR[,"YUR.l1"][j] <- predVAR[,"Level"][j-1]
#   }}


##Backtesting for ARIMAX
#loop for rolling estimation
for(i in 1:40){
  start <- 2004+(i-1)*1/12
  end <- n.end+(i-1)*1/12
  train_YUR <- window(df.TS[,'YUR'], start, end)
  train_GT <- window(df.TS[,'GT'], start, end)
  predARIMAX[i,"Actual"] <- window(df.TS[,'YUR'], end+1/12, end+1/12)
  predARIMAX[i,"Forecast"] <-forecast(Arima(train_YUR[-1], order=c(0,1,2), 
                                            xreg = diff(train_GT)),
                   h=1, xreg=diff(window(df.TS[,'GT'], end, end+1/12)))$mean
}

##Plot of Backtesting
fcast.all <- ts(cbind(predARIMAX,predVAR[,'Level']), start=c(2017,6), frequency = 12)
colnames(fcast.all) <- c("Actual","ARIMAX", "VAR")
#write.csv(fcast.all, "Forecast.csv")

autoplot(as.xts(fcast.all), facets=NULL) + xlab("Month") + 
  ylab("Youth Unemployment Rate") + theme_bw() + 
  ggtitle("Actual Vrs ARIMAX Vrs VAR")

##Compute OOT Error Metric
OOT <- matrix(rep(NA,2*5),2,5)
rownames(OOT) <- c("ARIMAX","VAR")
colnames(OOT) <- c("MSE","RMSE","MAE","MAPE","SMAPE")

library(Metrics)
library(scales)
for(i in 1:2){
  OOT[i,"MSE"] <- round(mse(fcast.all[,"Actual"],fcast.all[,1+i]),3)
  OOT[i,"RMSE"] <- round(rmse(fcast.all[,"Actual"],fcast.all[,1+i]),3)
  OOT[i,"MAE"] <- round(mae(fcast.all[,"Actual"],fcast.all[,1+i]),3)
  OOT[i,"MAPE"] <- percent(mape(fcast.all[,"Actual"],fcast.all[,1+i]), accuracy = 0.001)
  OOT[i,"SMAPE"] <- percent(smape(fcast.all[,"Actual"],fcast.all[,1+i]), accuracy = 0.001)
}
#write.csv(OOT, "OOT.csv")

# Compare Predictability of ARIMAX vs VAR
e1 <- fcast.all[,"Actual"]-fcast.all[,"ARIMAX"] 
e2 <- fcast.all[,"Actual"]-fcast.all[,"VAR"]
# compute Diebold-Mariano statistic
dm.test(e1, e2, h=1, power=2)


