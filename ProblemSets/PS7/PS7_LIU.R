install.packages("mice")
install.packages("stargazer")
install.packages("magrittr")
install.packages("dplyr")

library(magrittr) 
library(dplyr)
library(mice)
library(stargazer)
library(tidyverse)

# Question 4 Using R or Python, load the file wages.csv in as a data frame.
wage <-  read.csv("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/ModelingOptimization/wages.csv")
View(wage)

# Question 5 Drop observations where either hgc or tenure are missing.
wage_drop <- wage %>% drop_na(hgc,tenure)

# Question 6 Use stargazer to produce a summary table of this data frame.
stargazer(wage)
stargazer(wage_drop)

# Question 7.1 Estimate the regression using only complete cases.
wage_drop_MCAR <- wage_drop %>% drop_na(logwage)
Model_MCAR <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=wage_drop_MCAR)

# Question 7.2 Perform mean imputation to fill in missing log wages.
wage_drop_mean <- wage_drop
wage_drop_mean$logwage[which(is.na(wage_drop_mean$logwage))] <- mean(wage_drop_mean$logwage, na.rm = TRUE)
Model_mean <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=wage_drop_mean)

# Question 7.3 Impute missing log wages as their predicted values from the complete cases regression.
wage_drop_pred <- wage_drop
wage_drop_pred$logwage[is.na(wage_drop_pred$logwage)] <- predict(Model_MCAR, wage_drop_pred[is.na(wage_drop_pred$logwage),])
Model_pred <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=wage_drop_pred)

# Question 7.4 Use the mice package to perform a multiple imputation regression model.
library(mice)
data("wage_drop")
head(wage_drop)
wage_drop.imp <- mice(wage_drop, seed = 12345)
summary(wage_drop.imp)
fit <- with(wage_drop.imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
# while I write " round(summary(pool(fit)),2) ", there is an error: Error in Math.data.frame(list(term = 1:7, estimate = c(0.707595585226211,  : 
# non-numeric variable(s) in data frame: term.

# Question 7.5 Use stargazer to create one regression table which has the estimates of the first three regression models.
stargazer(Model_MCAR, Model_mean, Model_pred)
