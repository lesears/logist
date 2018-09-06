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
cntrpt <- mean(df_monthly$x)

# Plotting overlay of decomposition with original data for reference
#plot(wells, col = "black", main = "Well Depth - Trend/Cycle", xlab = "Year", ylab = "Corrected Measurement", lwd = 2)
#lines(decomp_stl$time.series[,1]+cntrpt, col = "red", lwd = 2) # Seasonal plot
#lines(decomp_stl$time.series[,2], col = "red", lwd = 2) # Trend plot
#lines(decomp_stl$time.series[,3]+cntrpt, col = "red", lwd = 2) # Remainder Plot
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

# Formatting time series output for use with plots
actual_data <- df_monthly
actual_data$year=paste('20',substr(actual_data$Group.1,3,4),sep="")
actual_data$month=substr(actual_data$Group.1,6,7)
actual_data$index <- as.factor(seq.int(nrow(actual_data)))
pred_1 <- as.data.frame(HWES.Well$fitted)
pred_1$index <- seq.int(nrow(pred_1))
pred_2 <- as.data.frame(HWES.Well$mean)
pred_2$index <- seq.int(nrow(pred_2))+123
pred_vals <- bind_rows(pred_1,pred_2)
trend_comp <- as.data.frame(decomp_stl$time.series[,2])
trend_comp$index <- seq.int(nrow(trend_comp))
season_comp <- as.data.frame(decomp_stl$time.series[,1]+cntrpt)
season_comp$index <- seq.int(nrow(season_comp))
train_plot <- actual_data[1:123,]

# Plotting desired results using GGPlot
# Actual vs. Predicted
ggplot()+
   geom_line(data = actual_data, size = 1,aes(color = "Actual",x = index, y = x, group=1)) +
   geom_line(data = pred_vals, size = 1, aes(color="Predicted",x=index, y = x, group = 1)) +
   labs(x = "Month", y = "Corrected Well Height (ft)", title = "Corrected Well Water Heights for Well G-860") +
   theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16)) + scale_x_discrete(breaks=seq(5,125,24),labels=c("5" = "Feb. 2008","29" = "Feb. 2010","53" = "Feb. 2012","77" = "Feb. 2014","101" = "Feb. 2016","125" = "Feb. 2018")) + scale_colour_manual("Parameters",values=c("#00AFBB","black"))

# Actual vs. Trend
ggplot()+
   geom_line(data = train_plot, size = 1,aes(color = "Actual",x = index, y = x, group=1)) +
   geom_line(data = trend_comp, size = 1, aes(color = "Trend",x=index, y = x, group = 1)) +
   labs(x = "Month", y = "Corrected Well Height (ft)", title = "Corrected Well Water Heights for Well G-860") +
   theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16)) + scale_x_discrete(breaks=c(5,27,50,72,95,118),labels=c("5" = "Feb. 2008","27" = "Dec. 2009","50" = "Nov. 2011","72" = "Sep. 2013","95" = "Aug. 2015","118" = "Jul. 2017")) + scale_colour_manual("Parameters",values=c("#00AFBB","black"))

# Actual vs. Seasonal
ggplot()+
   geom_line(data = train_plot, size = 1,aes(color = "Actual",x = index, y = x, group=1)) +
   geom_line(data = season_comp, size = 1, aes(color = "Seasonal", x=index, y = x, group = 1)) +
   labs(x = "Month", y = "Corrected Well Height (ft)", title = "Corrected Well Water Heights for Well G-860") +
   theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16)) + scale_x_discrete(breaks=c(5,27,50,72,95,118),labels=c("5" = "Feb. 2008","27" = "Dec. 2009","50" = "Nov. 2011","72" = "Sep. 2013","95" = "Aug. 2015","118" = "Jul. 2017")) + scale_colour_manual("Parameters",values=c("#00AFBB","black"))