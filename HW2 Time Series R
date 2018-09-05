setwd('C:\\Users\\Ryan Carr\\OneDrive\\Documents\\MSA\\Time Series\\')
library(readxl)
library(zoo)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(seasonal)

df <- read.csv("Time Corrected Well Data.csv", header = TRUE)

#change to date format
df$monthday <- as.Date(df$date , "%m/%d/%Y")

#take substring of month to remove day element
df$month <- substr(df$monthday, 1, 7)

#aggregate hourly data to monthly
df_monthly <- aggregate(df[, 6], list(df$month), mean)

#check for missing values - there are no missing
#values in either of our datasets
table(is.na(df))
table(is.na(df_monthly))

#create test set of last 6 months
df_test <- df_monthly[124:129,]
df_train <- df_monthly[1:123,]

#creation of time series object
wells <- ts(df_train$x, start=c(2007,4), frequency =12)

#time series decomposition
decomp_stl <- stl(wells, s.window = 7)
plot(decomp_stl)

plot(wells, col = "black", main = "Well Depth - Trend/Cycle", xlab = "", ylab = "Corrected Measurement", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

