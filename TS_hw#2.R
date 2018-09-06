#################################
#                               #
#       MSA Class of 2019       #
#                               #
#         Time Series           #
#          Homework 2           #
#        Orange Team 2          #
#                               #
#################################

library(readxl)
library(zoo)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(seasonal)
library(dplyr)
library(ggplot2)

setwd("/Users/matttrombley/Dropbox/Fall 2018/Fall 1/Time Series/HW2/")
input.file1 <- "Time_Corrected_Well_Data.csv"
df <- read.csv(input.file1, header = TRUE)

# Change to date format & remove day element, then aggregate to monthly
df$monthday <- as.Date(df$UTC.Date , "%m/%d/%Y")
df$month <- substr(df$monthday, 1, 7)
df_monthly <- aggregate(df[, 6], list(df$month), mean)

# Check for missing values - there are none
table(is.na(df))
table(is.na(df_monthly))

# Create training and validation data sets and time-series objects
df_test <- df_monthly[124:129,]
df_train <- df_monthly[1:123,]
wells <- ts(df_train$x, start=c(2007,10), frequency =12)
df_test_ts <- ts(df_test$x, start=c(2018,1),frequency=12)

# STL decomposition of training set
decomp_stl <- stl(wells, s.window = 7)
plot(decomp_stl)

# Plotting overlay of decomposition with original data
cntrpt <- mean(df_monthly$x)
plot(wells, col = "black", main = "Well Depth - Trend/Cycle", xlab = "Year", ylab = "Corrected Measurement", lwd = 2)
lines(decomp_stl$time.series[,1]+cntrpt, col = "red", lwd = 2) # Seasonal plot
lines(decomp_stl$time.series[,2], col = "red", lwd = 2) # Trend plot
lines(decomp_stl$time.series[,3]+cntrpt, col = "red", lwd = 2) # Remainder Plot
#lines(decomp_stl$time.series[,1]+decomp_stl$time.series[,2]+decomp_stl$time.series[,3], col = "red", lwd = 2) # Total Plot

# Use Holt-Winters additive ES for model creation
HWES.Well <- hw(wells, seasonal = "additive",h=6)
plot(HWES.Well,xlim=c(2007.92,2018.5))
test.results=forecast(HWES.Well,h=6)
error=df_test_ts-test.results$mean
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(df_test_ts))
round(accuracy(HWES.Well),2)

# Test variety of other models for comparison
# SES model
sestest <- ses(wells, initial = "optimal",h=6)
plot(sestest)
test.results_ses=forecast(sestest,h=6)
error_ses=df_test_ts-sestest$mean
MAE_ses=mean(abs(error_ses))
MAPE_ses=mean(abs(error_ses)/abs(df_test_ts))
round(accuracy(sestest),2)

# Holt model
holttest <- holt(wells, initial = "optimal",h=6)
plot(holttest)
test.results_holt=forecast(holttest,h=6)
error_holt=df_test_ts-holttest$mean
MAE_holt=mean(abs(error_holt))
MAPE_holt=mean(abs(error_holt)/abs(df_test_ts))
round(accuracy(holttest),2)


# Use ggplot to create nice visualizations here
test3 <- as.data.frame(HWES.Well$fitted)
test3$index <- seq.int(nrow(test3))
#test2$year=paste('20',substr(test2$Group.1,3,4),sep="")
#test2$month=substr(test2$Group.1,6,7)
#
test2$index <- seq.int(nrow(test2))
#
###############################################

ggplot()+
   geom_line(data = test2, color = "#00AFBB", size = 1,aes(x = index, y = x, group=1)) +
   geom_line(data = test3, size = 1,aes(x=index, y = x, group = 1)) +
   labs(x = "Month/Year", y = "Corrected Well Value", title = "Corrected Well Values for Well G-860") +
   theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+ scale_x_continuous(labels=c("0" = "Oct. 2007", "25" = "Oct. 2009", "50" = "Nov. 2011", "75" = "Dec. 2013", "100" = "Jan. 2016", "125" = "Feb. 2009"))