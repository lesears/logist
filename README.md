HW# LOGISTIC REGRESSION

Using only the training dataset, develop a report about your initial analysis of different factors that
influence whether or not the customer purchased the insurance product. For any hypothesis testing, pick
a value of α between 0.2 and 0.001, state it, and use it for this entire assignment. If you want to just use
α = 0.05, go for it.

Make sure that the report addresses the following issues:
Before you use the response for anything, look at the distributions of all of your predictors.
Are there any with a large proportion of missing values? Ignore these variables. Are there any that
have a very narrow distribution (e.g., almost entirely 0s or entirely 1s)? Consider ignoring these or
transforming/combining them in some sensible way if you can think of one. Feel free to examine any
crosstabulation tables between some sets of two predictors as well to see if anything jumps out.

```{r}
 install.packages('forecast',dependencies = T)
install.packages('tseries')
install.packages(c('expsmooth','lmtest','zoo','seasonal','haven','fma'))
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
file.dir <- "C:/Users/Laney/Documents/Data/data/"
input.file1 <- "insurance_t.sas7bdat"
input.file2 <- "insurance_v.sas7bdat"

insur_train <- read_sas(paste(file.dir, input.file1,sep = ""))
insur_valid <- read_sas(paste(file.dir, input.file2, sep = ""))

summary(insur_train)
table(is.na(insur_train))




```

Which of your predictors (continuous and categorical) do you think might be important to your problem?
Why? This can be based on subject knowledge, literature, test results, or whatever you feel might be
important. Fit a logistic regression model with these variables. (If you have no idea and are only going
off test results to decide what goes into your model, that’s fine.) Give an interpretation (including the
confidence interval) of the odds ratio for the predictor with the largest estimate (in magnitude).

```{r}
# fit GLMs using glm()
# family specifies data distribution, and link function goes in parentheses
#fit <- glm(low ~ age + lwt + smoke + race,
           #data = lowbwt, family = binomial(link = "logit"))
#summary(fit)

# get profile likelihood CI. if you load the MASS package, it will get the
# profile likelihood CIs by default
#exp(confint(fit)) # since we're exponentiating, these are the CIs for the odds
# ratios

```
Predictions
```{r}
### making predictions
# create a new dataset of the subjects we'd like to compare
# here, we're looking two 30-y/o, 130lb women, one is Black and smokes, the
# other is White and doesn't smoke
#newdata <- data.frame(age = c(30, 30),
                      #lwt = c(130, 130),
                      #race = c("black", "white"),
                      #smoke = c(1, 0))
# type = "link" will return the predicted log(odds) for each of these subjects
#predict(fit, newdata = newdata, type = "link")
# so to get the odds ratio, we need to exponentiate that
#exp(predict(fit, newdata = newdata, type = "link"))
# the diff() function is the second value minus the first
# so the following statement is comparing the White non-smoker
# to the Black smoker
#exp(diff(predict(fit, newdata = newdata, type = "link")))

# for probabilities, we need to use type = "response"
#predict(fit, newdata = newdata, type = "response")
#diff(predict(fit, newdata = newdata, type = "response"))

### plotting predicted probabilities
# visreg() will plot predicted probabilities for one predictor at whatever
# levels of the other predictors you set
# the first argument is your model
# the second is the predictor you're interested in
# the "by" argument will plot separate lines for each level of the "by" variable,
# and the "overlay" option will put them all on the same graph. so here, we'll
# get a line for each race
# scale = "response" tells it to plot the probabilities rather than log(odds)
# the "cond" argument is where you specify the levels of the other predictors.
# by default, if left unspecified, it will use the median of the continuous
# predictors and the most frequent category of categorical predictors
#visreg(fit, "age", by = "race", scale = "response",
       #cond = list(smoke = 0, lwt = 130),
       #overlay = TRUE)
```

Think of an interesting comparison involving multiple predictors. Compute and interpret the odds ratio
for these two subjects.

```{r}
### likelihood ratio test
# let's do the LRT comparing our original model to one without smoking and race
#fit2 <- glm(low ~ age + lwt, data = lowbwt, family = binomial)
#anova(fit, fit2, test = "LRT")
```

The dataset has several variables that might have redundant information (e.g., money market account
and money market balance) or might be indicative of the same underlying phenomenon (e.g., teller
visits and phone number banking could represent something like actual human contact with the bank).
Is anything like this in your model? If so, why do you feel like you need to keep both? (There’s no
right or wrong answer.)

```



```

How many of your predictors have missing values? Earlier, you ignored predictors with a large number
of missing values, which is a perfectly valid thing to do—the idea being that they might be likely to
be missing in the future as well and thus may not be useful for the application of your model.1 How
many observations have missing values? You should keep in mind and make a note of how much of
your sample is being discarded when we only do a complete case analysis. Dealing with missing values
is challenging to do accurately and beyond the scope of this class, so for now we won’t worry about it
aside from noting it here.

```

```
