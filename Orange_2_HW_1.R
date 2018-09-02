#################################
#                               #
#       MSA Class of 2019       #
#                               #
#      Logistic Regression      #
#          Homework 1           #
#        Orange Team 2          #
#                               #
#################################

#install.packages('forecast',dependencies = T)
#install.packages(c('expsmooth','lmtest','zoo','seasonal','haven','fma','gmodels','car','tseries'))
library(car)
library(gmodels)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
options(scipen=999) # Improve readability of parameter estimates

file.dir <- '/Users/matttrombley/Dropbox/Fall 2018/Fall 1/Logistic Regression/HW1/' # Replace with path to your file
input.file1 <- "insurance_t.sas7bdat"
insur_train <- read_sas(paste(file.dir, input.file1,sep = "")) # Read in training set
summary(insur_train) # Summarize to check for missing values and descriptive statistics

drop_vars <- c("CRSCORE", "ACCTAGE","PHONE", "POS", "POSAMT", "INV", "INVBAL", "CC", "CCBAL", "CCPURC", "INCOME", "HMOWN", "LORES", "HMVAL", "AGE") # Remove all columns having missing values
insur_train_drop_na <- insur_train[, -which(names(insur_train) %in% drop_vars)] # Drops variables with names matching the above variable
summary(insur_train_drop_na) # Verify dataset has no more NAs

hist(insur_train_drop_na$CASHBK) # Create histograms to check for distribution of values - replace variable as desired
sum(insur_train_drop_na$SDB)/8495 # Sum to see proportion of 0s/1s - mostly 0s has avg. close to 0, and vice versa for 1s

hist(insur_train_drop_na$LOCBAL) # Check balance values corresponding to dummy variables for narrow distributions as well
sum(insur_train_drop_na$SDB)/8495

drop_vars_2 <- c("CASHBK", "NSF", "NSFAMT", "IRA", "IRABAL", "LOC", "LOCBAL", "ILS", "ILSBAL", "MTG", "MTGBAL","MOVED", "INAREA", "SDB") # Drop the following variables due to narrow distributions
insur_train_final <- insur_train_drop_na[, -which(names(insur_train_drop_na) %in% drop_vars_2)] # Creating cleaned set for modeling

CrossTable(insur_train_drop_na$BRANCH, insur_train_drop_na$INS, prop.r=FALSE, prop.c=TRUE,
           prop.t=FALSE, prop.chisq=FALSE) # Create cross tabulation as necessary

insur_ref <- glm(INS ~ DDA + DDABAL + DEP + DEPAMT + CHECKS + DIRDEP + TELLER + SAV + SAVBAL + ATM + ATMAMT + CD + CDBAL + MM + MMCRED, data = insur_train_final, family = binomial(link = "logit")) # Create the reference model with all variables left excluding branch location and urban/suburban based on group discussion
summary(insur_ref)
vif(insur_ref) # Check for multicollinearity, remove variables as needed
# MMBAL was dropped because of collinearity with MM

insur_fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM,data = insur_train_final, family = binomial(link = "logit")) # Create reduced model and test to see which variables need to be removed
# Model was iterated upon in this step and this displays the final selected model
summary(insur_fit)

anova(insur_ref, insur_fit, test = "LRT") # Analyze LRT between full model and newly fit model

confint(insur_fit)
exp(confint(insur_fit)) # Since we're exponentiating, these are the CIs
exp(max(insur_fit$coefficients)) # MM has the largest estimate of 2.3207

new1 <- data.frame(DDA = c(1,1), DDABAL = c(563.42, 563.2), DEP = c(2.131,2.131), CHECKS = c(2,2), TELLER = c(0,0), SAV = c(0,0), SAVBAL = c(0,0),ATMAMT = c(119.9,119.9),CD=c(0,1),CDBAL=c(0,20000),MM=c(0,1), ATM=c(0,0)) # Create new test case to check for interesting observations and interpretation

exp(predict(insur_fit, newdata = new1, type = "link")) # Gets the prediction for each individual new test case.

exp(diff(predict(insur_fit, newdata = new1, type = "link"))) # Interpretation of test cases - An average person is 6.72 times less likely to have an annuity than someone who is average except for having a CD with balance $20,000 and an MM.

diff(predict(insur_fit, newdata = new1, type = "response")) # Probabilities

insur_train$All_NA <- apply(insur_train[,1:48], 1, anyNA)
sum(insur_train$All_NA) # None of our final predictors have missing values, but 3034 entries had a null value in one or more column.
