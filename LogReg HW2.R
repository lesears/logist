#################################
#                               #
#       MSA Class of 2019       #
#                               #
#      Logistic Regression      #
#          Homework 2           #
#        Orange Team 2          #
#                               #
#################################

install.packages(c('expsmooth','lmtest','zoo','seasonal','haven','fma','descr','forecast','tseries','gmodels','car','sas7bdat','brglm2','tidyverse','MASS','ROCR','DescTools','Hmisc'))
library(car)
library(gmodels)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(descr)
library(sas7bdat)
library(brglm2)
library(visreg)
library(ggplot2)
library(tidyverse)
library(MASS)
library(ROCR)
library(DescTools)
library(Hmisc)
options(scipen=999)

#file.dir <- 'C:\\Users\\Laney\\Documents\\Data\\data\\' # Replace with path to your file (Windows)
file.dir <- '/Users/matttrombley/Dropbox/Fall 2018/Fall 1/Logistic Regression/data/' # Replace with path to your file (Mac)
input.file1 <- "insurance_t.sas7bdat"
input.file2 <- "insurance_v.sas7bdat"

insur_train <- read.sas7bdat(paste(file.dir, input.file1,sep = "")) # Read in training set
insur_valid <- read.sas7bdat(paste(file.dir, input.file2, sep = ""))

summary(insur_train)
table(is.na(insur_train))
apply(insur_train, 2, function(col)sum(is.na(col))/length(col))
# For NAs, need to remove PHONE, POS, POSAMT, INV, INVBAL, CC, CCBAL, CCPURC, INCOME, HMOWN, LORES, HMVAL, AGE

drop_vars <- c("CRSCORE", "ACCTAGE","PHONE", "POS", "POSAMT", "INV", "INVBAL", "CC", "CCBAL", "CCPURC", "INCOME", "HMOWN", "LORES", "HMVAL", "AGE")
insur_train_drop_na <- insur_train[, -which(names(insur_train) %in% drop_vars)]
insur_valid_drop_na <- insur_valid[, -which(names(insur_valid) %in% drop_vars)]
# Drops variables with names matching the above variable
summary(insur_train_drop_na)

hist(insur_train_drop_na$CASHBK) # Create histograms to check for distribution of values
sum(insur_train_drop_na$SDB)/8495 # Sum to see proportion of 0s/1s

# Because of narrow distributions, drop CASHBK, NSF, IRA, LOC, ILS, MTG, MOVED, INAREA, SDB and also corresponding variables NSFAMT, IRABAL, LOCBAL, ILSBAL, MTGBAL

drop_vars_2 <- c("CASHBK", "NSF", "NSFAMT", "IRA", "IRABAL", "LOC", "LOCBAL", "ILS", "ILSBAL", "MTG", "MTGBAL","MOVED", "INAREA", "SDB")
insur_train_final <- insur_train_drop_na[, -which(names(insur_train_drop_na) %in% drop_vars_2)]
insur_valid_final <- insur_valid_drop_na[, -which(names(insur_valid_drop_na) %in% drop_vars_2)]

CrossTable(insur_train_drop_na$BRANCH, insur_train_drop_na$INS, prop.r=FALSE, prop.c=TRUE,prop.t=FALSE, prop.chisq=FALSE) # Create cross tabulation as necessary

insur_ref <- glm(INS ~ DDA + DDABAL + DEP + DEPAMT + CHECKS + DIRDEP + TELLER + SAV + SAVBAL + ATM + ATMAMT + CD + CDBAL + MM + MMCRED,data = insur_train_final, family = binomial(link = "logit")) # Create the reference model with all variables left excluding branch location and urban/suburban based on group discussion
summary(insur_ref)
vif(insur_ref) # Check for multicollinearity, remove variables as needed

# Drop MMBAL because of collinearity with MM

insur_fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM,data = insur_train_final, family = binomial(link = "logit")) # Create model using backwards selection and test to see which variables need to be removed
anova(insur_ref, insur_fit, test = "LRT") # Analyze LRT between full model and newly fit model
summary(insur_fit)
confint(insur_fit)
exp(max(insur_fit$coefficients)) # MM has the largest estimate of 2.3207
exp(confint(insur_fit)) # Since we're exponentiating, these are the CIs

keep_vars <- c("DDA", "DDABAL", "DEP", "CHECKS", "TELLER", "SAV", "SAVBAL", "ATMAMT", "CD", "CDBAL", "MM","ATM","INS")
compare_set <- insur_train_final[, which(names(insur_train_final) %in% keep_vars)] # Create final set with variables in the last model to use to create new test cases

# For the odds ratios, interepretations are: A person with a money market account is 2.32 as likely to buy the insurance product as someone who does not have a money market account.

summary(compare_set)
new1 <- data.frame(DDA = c(1,1), DDABAL = c(563.42, 563.2), DEP = c(2.131,2.131), CHECKS = c(2,2), TELLER = c(0,0), SAV = c(0,0), SAVBAL = c(0,0),ATMAMT = c(119.9,119.9),CD=c(0,1),CDBAL=c(0,20000),MM=c(0,1), ATM=c(0,0))

exp(predict(insur_fit, newdata = new1, type = "link")) # Gets the prediction for each individual new test case.

exp(diff(predict(insur_fit, newdata = new1, type = "link"))) # Interpretation of test cases - An average person is 6.72 times less likely to have an annuity than someone who is average except for having a CD with balance $20,000 and an MM.

# Probabilities
diff(predict(insur_fit, newdata = new1, type = "response"))

insur_train$All_NA <- apply(insur_train[,1:48], 1, anyNA)
sum(insur_train$All_NA)
# None of our final predictors have missing values.
# 3034 entries had a null value in one or more column.

insur_fit_sep <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM, data = insur_train_final, family = binomial(link = "logit"), method = "detect_separation") # Check for linear separation among the predictors
insur_fit_sep # No linear separation

insur_fit_subdda <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM, data = insur_train_final, family = binomial(link = "logit"),subset=DDABAL<25000) # Model on subset of DDABAL data

# Create partial residual plots to check for linearity of continuous predictors

visreg(insur_fit_subdda, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL subset DDABAL<25000",
       x = "DDABAL", y = "partial (deviance) residuals")
visreg(insur_fit, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")
visreg(insur_fit, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")
visreg(insur_fit, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")
visreg(insur_fit, "CDBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CDBAL",
       x = "CDBAL", y = "partial (deviance) residuals")

#Assumption of linearity for our continuous variables seems to be ok - some seem nonlinear for large values, but it's because of influential observations
# Make note that all of our continuous variables are extremely right-skewed

#Adding interaction between our top two main effects, MM and CD

insur_fit1 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM + MM*CD, data = insur_train_final, family = binomial(link = "logit"))
insur_fit1.a <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM + MM*CD+MM*SAV, data = insur_train_final, family = binomial(link = "logit"))
insur_fit2 <-  glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM, data = insur_train_final, family = binomial(link = "logit"))

anova(insur_fit1, insur_fit, test = "LRT") # Have to flip order of models for LRT
cat("AIC for interactions",AIC(insur_fit1),"\nAIC for base",AIC(insur_fit))

#AIC worse when you add CD*SAV, p value = 0.5, but AIC better when you add MM*SAV, p -value = 0.0002883
anova(insur_fit1, insur_fit2, test = "LRT")
cat("AIC for interactions",AIC(insur_fit2),"\nAIC for base",AIC(insur_fit1))

# Best model involving interaction terms = insur_fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM + MM*CD+MM*SAV, data = insur_train_final, family = binomial(link = "logit"))

# Calibration curve to check interaction validity
calib.plot <- function(model){
  # function to plot a calibration curve
  # inputs:
  # model = fitted glm() model
  # outputs: a plot of observed vs. predicted probabilities with loess smooth
  # create data frame of outcomes and predicted probabilities
  pred <- data.frame(y = model$y, phat = fitted(model))
  pred <- dplyr::arrange(pred, phat) # sort by prob in ascending order
  # pred$phat.loess <- fitted(loess(y ~ phat, data = pred))
  # pred$phat.loess.trunc <- pmax(pmin(pred$phat.loess, 0.9999), 0.0001)
  # finding max x-axis value to display
  max_p <- min(c((round(max(pred$phat), digits = 1) + 0.1), 1)) # plotting
  ggplot(data = pred) +
    geom_point(mapping = aes(x = phat, y = y), color = "black") +
    geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
    geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
    labs(x = "predicted probability", y = "observed proportion") +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, max_p)) +
    theme_bw()
}
calib.plot(insur_fit) + labs(title = "calibration curve")
calib.plot(insur_fit1) + labs(title = "calibration curve") # Try a model with some interactions
calib.plot(insur_fit2) + labs(title = "calibration curve")

# Final model: remove the interactions, i.e. = insur_fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM , data = insur_train_final, family = binomial(link = "logit"))

influence.measures(insur_fit) # Check for measures of influence and investigate
plot(insur_fit, 4, n.id = 5)
outlier_index = c(1547, 1721, 4601)
compare_set[outlier_index,]
summary(compare_set)

# Create dfbetas plots for continuous predictors

dfbetasPlots(insur_fit, terms = "DDABAL", id.n = 5,
             col = ifelse(insur_fit$y == 1, "red", "blue"))
dfbetasPlots(insur_fit, terms = "SAVBAL", id.n = 5,
             col = ifelse(insur_fit$y == 1, "red", "blue"))
dfbetasPlots(insur_fit, terms = "ATMAMT", id.n = 5,
             col = ifelse(insur_fit$y == 1, "red", "blue"))
dfbetasPlots(insur_fit, terms = "CDBAL", id.n = 5,
             col = ifelse(insur_fit$y == 1, "red", "blue"))

insur_v <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER + SAV + SAVBAL + ATMAMT + CD + CDBAL + MM + ATM,data = insur_train_final, family = binomial(link = "logit")) # Final model creation for use for prediction

predicted<- predict(insur_v, newdata = insur_valid_final, type="response")
pred <- prediction(predicted, insur_valid_final$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                         tpr = perf@y.values[[1]],
                         tnr = 1 - perf@x.values[[1]])
classif_table$youdenJ <- with(classif_table, tpr + tnr - 1) # Youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table[which.max(classif_table$youdenJ),] # Find row with max

# Actual classification table
df <- data.frame(y = insur_valid_final$INS,
                 phat = predicted)
df_new<- data.frame(y = insur_valid_final$INS,
                 yhat = predicted)
for (i in 1:length(df$phat)){
  if (df$phat[i]>= classif_table[which.max(classif_table$youdenJ),1]) {
    df_new$yhat[i]=1
  }
  else{
    df_new$yhat[i]=0
  }
}
xtabs(~ yhat+ y, data = df_new) # Youden = 0.406, threshold = 0.317, tpr = 0.752, tnr 0.654

D <- mean(fitted(insur_v)[insur_v$y == 1]) - mean(fitted(insur_v)[insur_v$y == 0])
D #discrimination - 0.165

# Check for c-statistic and Somer's D - predicted prob goes first, outcome second
rcorr.cens(fitted(insur_v), insur_v$y)[-c(5, 6, 9)] # Ignoring unnecessary output
# c-stat =0.756, somers D = 0.512
# For all possible (1,0) pairs, the model assigned the higher predictive probability to the 1 76% of the time.

perf <- performance(pred, measure = "tpr", x.measure = "fpr") # Creating ROC curves
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
auc <- performance(pred, measure = "auc")@y.values # Find AUC for ROC curve
auc

brier_score <- function(obj, new_x = NULL, new_y = NULL){
  # computes [scaled] brier score
  # inputs:
  # 1. obj: either a model from glm() or a data frame.
  #         the data frame must have a vector responses "y" and a vector of
  #         either probabilities "p" or linear predictor "lp"
  #         (NOTE: for now only )
  # 2. new_x: specify new dataset to get predicted probabilities for new obs.
  #             if NULL, the estimated probabilities from original obs will
  #             be used.
  # 3. new_y: use new responses. if NULL, original ones will be used.
  # output:
  #   brier score, max brier score, scaled brier score
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  p_obs <- mean(y)
  p <- predict(obj, newdata = new_x, type = "response")
  # brier score
  brier_score <- mean((y - p)^2, na.rm = TRUE)
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  res <- data.frame(brier_score = brier_score,
                   brier_max = brier_max,
                   brier_scaled = brier_scaled)
  res
}

brier_score(insur_v, insur_valid_final, insur_valid_final$INS)
# Brier Score - 0.865
