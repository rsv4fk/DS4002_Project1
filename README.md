# DS4002_Project1

## Contents
1. SRC
2. Data
3. Figures
4. References
   
## SRC
### Installing Code 

We used R version 4.4.2. The packages we used were tidyverse version 1.3.2, dplyr version 1.0.10, stringr version 1.5.0, and berryFunctions version 1.22.0.

### Using Code
We began by cleaning the data and creating our dataset. We created a Date variable, which included the month and year of the publication of the article. Using this variable, merged the New York Times dataset with CPI and unemployment data from the U.S. Bureau of Labor Statistics. We removed variables in the initial dataset that were not pertinent to our research, and we filtered the type of publication to articles because we were looking at news coverage. 
   
We focused on searching the abstract, lead paragraph, keywords, main headline, and print line to find the counts of the words "virus," "corona," "covid," "pandemic," and "epidemic." To do this, we created an if-else statement within a for loop for each month, which searched the abstract, lead paragraph, keywords, main headline, and print headline of each record until it found one of these words: "virus," "corona," "covid," "pandemic," or "epidemic." Using this count, we created a variable of the percentage of articles published that mentioned COVID for each month, which we added to the dataset. 

We then merged the data set with data on the number of monthly COVID cases from *******, filtering for all the cases located in the United States.

To analyze the variables that affect unemployment rate, we found the correlation between COVID covereage frequency and unemployment, CPI and unemployment, number of COVID cases and unemployment, and the percentage change in the number of cases and unemployment. 

We then conducted regresssions, beginning with news coverage as the only variable predeicting unemployment. To control for the number of COVID casess, we created a model adding the number of COVID cases per month to the original model as a predictor, and then we created another model adding percent change in COVID cases per month as another predictor. 

Because the variable for percent change in COVID cases was significant, we added it to the model and also added CPI to control for economic changes. Finally, we conducted a regression with coverage frequency and the percent change in cases as predictors of the unemployment rate. 
   
## Data
| Variable     | Description | 
|--------------|:-----:|
| Date | year-month |   
| coverage_freq | the percentage of articles that mention COVID out of all the articles published that month
| Abstract | A brief overview of the article |  
| lead_paragraph | The first paragraph of the article |
| keywords | Important words related to the article |
| pub_date | The date the article was published |
| type_of_material | The format of the article |
| headline.main | The title of the article |
| headline.print_headline | The title of the article in print |
| unemployment.rate | Unemployment level of civilians aged 16 years and over |
| CPI | Consumer price index |
| case_count | The number of COVID cases in each month |
| Percent_change_cases | The percentage change in the number of COVID cases each month |

We created a dataset containing information on New York Times publications from January 2020 to November 2022. We also added economic indicators, unemployment rate and CPI, from the U.S. Bureau of Labor Statistics for each month. We also added the number of COVID cases per month from Our World in Data. 

The datasets can be found at this link. 
[https://drive.google.com/file/d/16SsuePp1FNpH-O0FbY62PyNdL17Y2AYU/view?usp=sharing](https://drive.google.com/drive/folders/1vLIIAFn1NoAg1cpILR5uDy63BM465rL6?usp=sharing)

## Figures
| Figure     | Value | Summary |
|--------------|:-----:|:-----:|
| correlation | 0.818 | There is a strong, positive relationship between coverage frequency and unemployment rate. |
| correlation | -0.658 | There is a moderate, negative relationship between CPI and unemployment rate. |
| correlation | -0.179| There is a weak, negative relationship between case count and unemployment rate. |
| correlation | -0.084| There is a very weak, negative relationship between the percent change in cases and unemployment rate. |
| p-value | 1.92e-09 | Coverage frequency is a statistically significant predictor of unemployment rate at a significance level of 0.001. |
| p-value| 0.130212 | Case count is not a statistically significant predictor of unemployment rate at a significance level of 0.001. | 
| p-value | 9.80e-07 | The percentage change in cases is a statistically significant predictor of unemployment rate at a significance level of 0.001. |
| p-value | 0.876 | CPI is not a statistically significant predictor of unemployment rate at a significance level of 0.001. | 
| coefficient | 1.930e-01 | In the final model, a 1% increase in COVID news coverage will lead to a 0.193% increase in the unemployment rate, holding the percentage change in cases constant. |
| coefficient | -2.732e-05 | In the final model, a 1% increase in the percentage change in COVID cases will lead to a 0.00002732% decrease in the unemployment rate, holding the COVID news coverage constant.
| adjusted R-squared | 0.8359 | 83.59% of the variation in the unemployment rate can be explained by news coverage of COVID as well as the percent change in cases each month. | 

## References

“CPI Home,” U.S. Bureau of Labor Statistics, https://www.bls.gov/cpi/ (accessed Sep. 13, 2023). 

“Labor Force Characteristics (CPS),” U.S. Bureau of Labor Statistics, https://www.bls.gov/cps/lfcharacteristics.htm#unemp (accessed Sep. 15, 2023). 

‌L. Mutikani, “US unemployment rate spikes to 3.8%; labor market still has momentum,” Reuters, Sep. 01, 2023. Available: https://www.reuters.com/markets/us/us-job-growth-picks-up-august-wages-gains-slow-2023-09-01/	

U.S. Bureau of Labor Statistics, “Civilian unemployment rate,” Bls.gov, 2023. https://www.bls.gov/charts/employment-situation/civilian-unemployment-rate.htm

Milestones: https://drive.google.com/drive/folders/1yCSYnEMdO9pQjDZjuaGB9ObXnRE6r9iZ?usp=sharing
