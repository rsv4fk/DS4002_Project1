##################
# DATA ANALYSIS #
#################

############
# PACKAGES #
############

library(tidyverse)
library(dplyr)
library(stringr)
library(berryFunctions)

#################
# ANALYTIC FILE #
#################

AnalyticFile <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\AnalyticFile.csv")

################
# CORRELATIONS #
################

cor(AnalyticFile$coverage_freq, AnalyticFile$Unemployment.Rate) # 0.8183232 -- a reasonably strong, positive relationship
cor(AnalyticFile$CPI, AnalyticFile$Unemployment.Rate) # -0.6578328 -- a moderate-to-strong, negative relationship
cor(AnalyticFile$case_count, AnalyticFile$Unemployment.Rate) # -0.1788494 -- a weak, negative relationship
cor(AnalyticFile$Percent_change_cases, AnalyticFile$Unemployment.Rate) # -0.08425265 -- an extremely weak, near-zero negative relationship

###############
# REGRESSIONS #
###############

# Step 1: Model with only the news coverage variable
model1 <- lm(Unemployment.Rate~coverage_freq, data=AnalyticFile)
summary(model1) # the coverage_freq variable is statistically significant

# Step 2: Models with the news coverage variable as well as controls for number of cases
##### Model 2.1 includes the number of COVID cases per month
##### Model 2.2 includes the percent change in number of COVID cases per month

model2.1 <- lm(Unemployment.Rate~coverage_freq+case_count, data=AnalyticFile) 
model2.2 <- lm(Unemployment.Rate~coverage_freq+Percent_change_cases, data=AnalyticFile)
summary(model2.1) # the case_count variable is not statistically significant
summary(model2.2) # the Percent_change_cases variable is statistically significant

# We see that the additional control variable in Model 2.2, Percent_change_cases, is significant, so we add it to the model

# Step 3: Model with the news coverage variable as well as controls for the monthly percent change in number of cases and the CPI
model3 <- lm(Unemployment.Rate~coverage_freq+Percent_change_cases+CPI, data=AnalyticFile)
summary(model3) # the CPI variable is not statistically significant

# We see that the additional control variable in Model 3, CPI, is not significant, so we can remove it from the model
model <- lm(Unemployment.Rate~coverage_freq+Percent_change_cases, data=AnalyticFile)
summary(model)

# This final model contains only the variables which which were found to be statistically significant predictors of the Unemployment Rate
# These variables, coverage_freq and Percent_change_cases, were both significant at a level alpha = 0.001
# The coefficient for coverage_freq is 0.1930, while the coefficient for Percent_change_cases is -0.00002732
# This implies that the amount of COVID news coverage has more of an impact on the unemployment rate
# Additionally, the adjusted R-squared value for this model was 0.8359, meaning that 83.59% of the variation in the Unemployment Rate can be explained by news coverage of COVID as well as the percent change in cases each month


# The results of the above statistical analyses imply that the model is reasonably accurate. 
# To validate this claim (and to test the model manually) we set up several prediction intervals and used the true values of the level of COVID news coverage and the percent change in number of cases throughout the time period from January 2020 to November 2022 as inputs to test the accuracy of our model

# Inputs are real data from March 2020, when the unemployment rate was 4.4% 
prediction_data1 <- data.frame(cbind("coverage_freq"=54.143409, "Percent_change_cases"=274620.634921))
predict(model, prediction_data1, interval="predict")
# Based on our model, the predicted unemployment rate when the level of COVID news coverage and the percent change in number of cases is the same as it was in March 2020 is 4.4%
# Our model was able to accurately predict the unemployment rate for March 2020

# Inputs are real data from May 2020, when the unemployment rate was 13.2%
prediction_data2 <- data.frame(cbind("coverage_freq"=54.310553, "Percent_change_cases"=-11.905204))
predict(model, prediction_data2, interval="predict")
# Based on our model, the predicted unemployment rate when the level of COVID news coverage and the percent change in number of cases is the same as it was in May 2020 is 11.9%
# Our model was somewhat able to predict the unemployment rate for May 2020. Though the exact prediction was different by 1.3%, the prediction interval was still able to accurately capture the true unemployment rate for May 2020

# Inputs are real data from July 2021, when the unemployment rate was 5.4%
prediction_data3 <- data.frame(cbind("coverage_freq"=22.085736, "Percent_change_cases"=256.037654))
predict(model, prediction_data3, interval="predict")
# Based on our model, the predicted unemployment rate when the level of COVID news coverage and the percent change in number of cases is the same as it was in July 2021 is 5.7%
# Our model was fairly accurate in predicting the unemployment rate for July 20221 The exact prediction was different by only 0.3%, and the prediction interval was still able to accurately capture the true unemployment rate for July 2021

# Inputs are real data from July 2022, when the unemployment rate was 3.5% (the same as the pre-COVID unemployment rates seen in Jan and Feb 2020)
prediction_data4 <- data.frame(cbind("coverage_freq"=8.143780, "Percent_change_cases"=27.926201))
predict(model, prediction_data4, interval="predict")
# Based on our model, the predicted unemployment rate when the level of COVID news coverage and the percent change in number of cases is the same as it was in July 2022 is 3.0%
# Our model was fairly accurate in predicting the unemployment rate for July 2022. The exact prediction was different by only 0.5%, and the prediction interval was still able to accurately capture the true unemployment rate for July 2022
