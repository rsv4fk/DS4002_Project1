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
