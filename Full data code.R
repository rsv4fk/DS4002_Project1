#####################
### ALL DATA WORK ###
#####################

############
# PACKAGES #
############
library(tidyverse)
library(dplyr)
library(stringr)
library(berryFunctions)

#############
# RAW FILES #
#############

rawdata_nyt <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\nyt.csv")
rawdata_bls_unemp <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\UNEMP_BLS.csv")
rawdata_bls_cpi <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\CPI_BLS.csv")
rawdata_cases <- na.omit(read.csv("C:\\Users\\ravza\\Downloads\\full_data.csv"))

##########################
# ANALYTIC FILE CREATION #
##########################

# Add a column "Date" containing the month and year of publication
nyt_month_year <- rawdata_nyt %>% mutate(Date = case_when(str_detect(pub_date, "2020-01") ~ "2020-01",
                                                          str_detect(pub_date, "2020-02") ~ "2020-02",
                                                          str_detect(pub_date, "2020-03") ~ "2020-03",
                                                          str_detect(pub_date, "2020-04") ~ "2020-04",
                                                          str_detect(pub_date, "2020-05") ~ "2020-05",
                                                          str_detect(pub_date, "2020-06") ~ "2020-06",
                                                          str_detect(pub_date, "2020-07") ~ "2020-07",
                                                          str_detect(pub_date, "2020-08") ~ "2020-08",
                                                          str_detect(pub_date, "2020-09") ~ "2020-09",
                                                          str_detect(pub_date, "2020-10") ~ "2020-10",
                                                          str_detect(pub_date, "2020-11") ~ "2020-11",
                                                          str_detect(pub_date, "2020-12") ~ "2020-12",
                                                          str_detect(pub_date, "2021-01") ~ "2021-01",
                                                          str_detect(pub_date, "2021-02") ~ "2021-02",
                                                          str_detect(pub_date, "2021-03") ~ "2021-03",
                                                          str_detect(pub_date, "2021-04") ~ "2021-04",
                                                          str_detect(pub_date, "2021-05") ~ "2021-05",
                                                          str_detect(pub_date, "2021-06") ~ "2021-06",
                                                          str_detect(pub_date, "2021-07") ~ "2021-07",
                                                          str_detect(pub_date, "2021-08") ~ "2021-08",
                                                          str_detect(pub_date, "2021-09") ~ "2021-09",
                                                          str_detect(pub_date, "2021-10") ~ "2021-10",
                                                          str_detect(pub_date, "2021-11") ~ "2021-11",
                                                          str_detect(pub_date, "2021-12") ~ "2021-12",
                                                          str_detect(pub_date, "2022-01") ~ "2022-01",
                                                          str_detect(pub_date, "2022-02") ~ "2022-02",
                                                          str_detect(pub_date, "2022-03") ~ "2022-03",
                                                          str_detect(pub_date, "2022-04") ~ "2022-04",
                                                          str_detect(pub_date, "2022-05") ~ "2022-05",
                                                          str_detect(pub_date, "2022-06") ~ "2022-06",
                                                          str_detect(pub_date, "2022-07") ~ "2022-07",
                                                          str_detect(pub_date, "2022-08") ~ "2022-08",
                                                          str_detect(pub_date, "2022-09") ~ "2022-09",
                                                          str_detect(pub_date, "2022-10") ~ "2022-10",
                                                          str_detect(pub_date, "2022-11") ~ "2022-11",
                                                          str_detect(pub_date, "2022-12") ~ "2022-12"))

# Merge the nyt_month_year data with CPI and unemployment data from the Bureau of Labor Statistics 
merge1 <- merge(nyt_month_year, rawdata_bls_cpi)
merge2 <- merge(merge1, rawdata_bls_unemp)

# Create the RawAnalyticFile which will then be cleaned to remove irrelevant columns and non-"article" observations
RawAnalyticFile <- write.csv(merge2, "C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\RawAnalyticFile.csv")

##########################
# ANALYTIC FILE CLEANING #
##########################

raw_nyt_bls_data <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\RawAnalyticFile.csv")

# Remove unnecessary columns
raw_nyt_bls_data1 <- raw_nyt_bls_data[,-c(1, 4, 5, 7, 8, 9, 13, 14, 16, 17, 18, 21)]

# Remove any publication that isn't an article (e.g., video, podcast)
raw_nyt_bls_data2 <- raw_nyt_bls_data1 %>% filter(document_type == "article")

# Remove the "document_type" column as it is no longer needed
raw_nyt_bls_data3 <- raw_nyt_bls_data2[,-6]

# Ensure that "raw_nyt_bls_data3" is a dataframe (and not, for example, a tibble) and create a copy
raw_nyt_bls_data4 <- as.data.frame(raw_nyt_bls_data3)

# Convert all text in the dataframe to lowercase letters to simplify the data analysis process
for(x in 1:10){
  raw_nyt_bls_data4[,x] <- tolower(raw_nyt_bls_data4[,x])
}

#################
# TEXT ANALYSIS #
#################

# Create counters for every "Date" category

count_2020_01 <- 0
count_2020_02 <- 0
count_2020_03 <- 0
count_2020_04 <- 0
count_2020_05 <- 0
count_2020_06 <- 0
count_2020_07 <- 0
count_2020_08 <- 0
count_2020_09 <- 0
count_2020_10 <- 0
count_2020_11 <- 0
count_2020_12 <- 0
count_2021_01 <- 0
count_2021_02 <- 0
count_2021_03 <- 0
count_2021_04 <- 0
count_2021_05 <- 0
count_2021_06 <- 0
count_2021_07 <- 0
count_2021_08 <- 0
count_2021_09 <- 0
count_2021_10 <- 0
count_2021_11 <- 0
count_2021_12 <- 0
count_2022_01 <- 0
count_2022_02 <- 0
count_2022_03 <- 0
count_2022_04 <- 0
count_2022_05 <- 0
count_2022_06 <- 0
count_2022_07 <- 0
count_2022_08 <- 0
count_2022_09 <- 0
count_2022_10 <- 0
count_2022_11 <- 0

# Count how many articles contain the words "covid", "corona", "virus", "pandemic", or "epidemic" for each month and update the counters
for(x in 1:4199){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_01 <- count_2020_01 + 1
  }else{
    count_2020_01 <- count_2020_01 + 0}
}

for(x in 4200:8145){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_02 <- count_2020_02 + 1
  }else{
    count_2020_02 <- count_2020_02 + 0}
}

for(x in 8146:12803){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_03 <- count_2020_03 + 1
  }else{
    count_2020_03 <- count_2020_03 + 0}
}

for(x in 12804:17556){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_04 <- count_2020_04 + 1
  }else{
    count_2020_04 <- count_2020_04 + 0}
}

for(x in 17556:21697){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_05 <- count_2020_05 + 1
  }else{
    count_2020_05 <- count_2020_05 + 0}
}

for(x in 21698:25778){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_06 <- count_2020_06 + 1
  }else{
    count_2020_06 <- count_2020_06 + 0}
}

for(x in 25779:29905){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_07 <- count_2020_07 + 1
  }else{
    count_2020_07 <- count_2020_07 + 0}
}

for(x in 29906:33961){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_08 <- count_2020_08 + 1
  }else{
    count_2020_08 <- count_2020_08 + 0}
}

for(x in 33962:38250){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_09 <- count_2020_09 + 1
  }else{
    count_2020_09 <- count_2020_09 + 0}
}

for(x in 38251:42994){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_10 <- count_2020_10 + 1
  }else{
    count_2020_10 <- count_2020_10 + 0}
}

for(x in 42995:47125){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_11 <- count_2020_11 + 1
  }else{
    count_2020_11 <- count_2020_11 + 0}
}

for(x in 47126:50993){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2020_12 <- count_2020_12 + 1
  }else{
    count_2020_12 <- count_2020_12 + 0}
}

for(x in 50994:55091){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_01 <- count_2021_01 + 1
  }else{
    count_2021_01 <- count_2021_01 + 0}
}

for(x in 55092:58941){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_02 <- count_2021_02 + 1
  }else{
    count_2021_02 <- count_2021_02 + 0}
}

for(x in 58942:63234){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_03 <- count_2021_03 + 1
  }else{
    count_2021_03 <- count_2021_03 + 0}
}

for(x in 63235:67354){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_04 <- count_2021_04 + 1
  }else{
    count_2021_04 <- count_2021_04 + 0}
}

for(x in 67355:71314){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_05 <- count_2021_05 + 1
  }else{
    count_2021_05 <- count_2021_05 + 0}
}

for(x in 71315:75196){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_06 <- count_2021_06 + 1
  }else{
    count_2021_06 <- count_2021_06 + 0}
}

for(x in 75197:79185){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_07 <- count_2021_07 + 1
  }else{
    count_2021_07 <- count_2021_07 + 0}
}

for(x in 79186:82957){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_08 <- count_2021_08 + 1
  }else{
    count_2021_08 <- count_2021_08 + 0}
}

for(x in 82958:86884){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_09 <- count_2021_09 + 1
  }else{
    count_2021_09 <- count_2021_09 + 0}
}

for(x in 86885:90862){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_10 <- count_2021_10 + 1
  }else{
    count_2021_10 <- count_2021_10 + 0}
}

for(x in 90863:94600){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_11 <- count_2021_11 + 1
  }else{
    count_2021_11 <- count_2021_11 + 0}
}

for(x in 94601:98217){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2021_12 <- count_2021_12 + 1
  }else{
    count_2021_12 <- count_2021_12 + 0}
}

for(x in 98218:101780){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_01 <- count_2022_01 + 1
  }else{
    count_2022_01 <- count_2022_01 + 0}
}

for(x in 101781:105595){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_02 <- count_2022_02 + 1
  }else{
    count_2022_02 <- count_2022_02 + 0}
}

for(x in 105596:109600){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_03 <- count_2022_03 + 1
  }else{
    count_2022_03 <- count_2022_03 + 0}
}

for(x in 109601:113333){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_04 <- count_2022_04 + 1
  }else{
    count_2022_04 <- count_2022_04 + 0}
}

for(x in 113334:117083){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_05 <- count_2022_05 + 1
  }else{
    count_2022_05 <- count_2022_05 + 0}
}

for(x in 117084:120943){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_06 <- count_2022_06 + 1
  }else{
    count_2022_06 <- count_2022_06 + 0}
}

for(x in 120944:124504){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_07 <- count_2022_07 + 1
  }else{
    count_2022_07 <- count_2022_07 + 0}
}

for(x in 124505:127996){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_08 <- count_2022_08 + 1
  }else{
    count_2022_08 <- count_2022_08 + 0}
}

for(x in 127997:131770){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_09 <- count_2022_09 + 1
  }else{
    count_2022_09 <- count_2022_09 + 0}
}

for(x in 131771:135411){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_10 <- count_2022_10 + 1
  }else{
    count_2022_10 <- count_2022_10 + 0}
}

for(x in 135412:137061){
  if(grepl("virus", raw_nyt_bls_data4[x, 2]) | 
     grepl("virus", raw_nyt_bls_data4[x, 3]) |
     grepl("virus", raw_nyt_bls_data4[x, 4]) |
     grepl("virus", raw_nyt_bls_data4[x, 7]) |
     grepl("virus", raw_nyt_bls_data4[x, 8]) |
     grepl("corona", raw_nyt_bls_data4[x, 2]) |
     grepl("corona", raw_nyt_bls_data4[x, 3]) |
     grepl("corona", raw_nyt_bls_data4[x, 4]) |
     grepl("corona", raw_nyt_bls_data4[x, 7]) |
     grepl("corona", raw_nyt_bls_data4[x, 8]) |
     grepl("covid", raw_nyt_bls_data4[x, 2]) |
     grepl("covid", raw_nyt_bls_data4[x, 3]) |
     grepl("covid", raw_nyt_bls_data4[x, 4]) |
     grepl("covid", raw_nyt_bls_data4[x, 7]) |
     grepl("covid", raw_nyt_bls_data4[x, 8]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 2]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 3]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 4]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 7]) |
     grepl("pandemic", raw_nyt_bls_data4[x, 8]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 2]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 3]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 4]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 7]) |
     grepl("epidemic", raw_nyt_bls_data4[x, 8])){
    count_2022_11 <- count_2022_11 + 1
  }else{
    count_2022_11 <- count_2022_11 + 0}
}

# Determine the total number of articles for each month
monthly_totals <- as.data.frame(raw_nyt_bls_data4 %>% count(Date))[,2]

# Determine the COVID-coverage frequency as a percentage for each month
count_totals <- c(count_2020_01, count_2020_02, count_2020_03, count_2020_04, count_2020_05, count_2020_06, count_2020_07, count_2020_08, count_2020_09, count_2020_10, count_2020_11, count_2020_12, count_2021_01, count_2021_02, count_2021_03, count_2021_04, count_2021_05, count_2021_06, count_2021_07, count_2021_08, count_2021_09, count_2021_10, count_2021_11, count_2021_12, count_2022_01, count_2022_02, count_2022_03, count_2022_04, count_2022_05, count_2022_06, count_2022_07, count_2022_08, count_2022_09, count_2022_10, count_2022_11)
coverage_freq <- 100*(count_totals/monthly_totals)

# Create a dataset including both the level of news coverage and the Date variables
coverage_data <- as.data.frame(coverage_freq) %>% mutate(Date = c("2020-01",
                                                                  "2020-02",
                                                                  "2020-03",
                                                                  "2020-04",
                                                                  "2020-05",
                                                                  "2020-06",
                                                                  "2020-07",
                                                                  "2020-08",
                                                                  "2020-09",
                                                                  "2020-10",
                                                                  "2020-11",
                                                                  "2020-12",
                                                                  "2021-01",
                                                                  "2021-02",
                                                                  "2021-03",
                                                                  "2021-04",
                                                                  "2021-05",
                                                                  "2021-06",
                                                                  "2021-07",
                                                                  "2021-08",
                                                                  "2021-09",
                                                                  "2021-10",
                                                                  "2021-11",
                                                                  "2021-12",
                                                                  "2022-01",
                                                                  "2022-02",
                                                                  "2022-03",
                                                                  "2022-04",
                                                                  "2022-05",
                                                                  "2022-06",
                                                                  "2022-07",
                                                                  "2022-08",
                                                                  "2022-09",
                                                                  "2022-10",
                                                                  "2022-11"
))

#################################
# SECOND ANALYTIC FILE CREATION #
#################################

# Merge the coverage variable with the cleaned version of the raw_nyt_bls_data dataset
raw_nyt_coverage_bls_data <- merge(coverage_data, raw_nyt_bls_data4)

nyt_coverage_bls_data <- merge(coverage_data, rawdata_bls_unemp)
nyt_coverage_bls_data <- merge(nyt_coverage_bls_data, rawdata_bls_cpi)

# Create the RaqAnalyticFile2 which will then be merged with data on the number of monthly COVID cases 
RawAnalyticFile2 <- write.csv(raw_nyt_coverage_bls_data, "C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\RawAnalyticFile2.csv")
SmallAnalyticFile2 <- write.csv(nyt_coverage_bls_data, "C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\SmallAnalyticFile2.csv")

###############################
# MAIN ANALYTIC FILE CREATION #
###############################

# Remove any observation that isn't for the United States
raw_US_case_data <- rawdata_cases %>% filter(location == "United States")

# Add a column "Date" containing the month and year
US_case_data <- raw_US_case_data %>% mutate(Date = case_when(str_detect(date, "2020-01") ~ "2020-01",
                                                             str_detect(date, "2020-02") ~ "2020-02",
                                                             str_detect(date, "2020-03") ~ "2020-03",
                                                             str_detect(date, "2020-04") ~ "2020-04",
                                                             str_detect(date, "2020-05") ~ "2020-05",
                                                             str_detect(date, "2020-06") ~ "2020-06",
                                                             str_detect(date, "2020-07") ~ "2020-07",
                                                             str_detect(date, "2020-08") ~ "2020-08",
                                                             str_detect(date, "2020-09") ~ "2020-09",
                                                             str_detect(date, "2020-10") ~ "2020-10",
                                                             str_detect(date, "2020-11") ~ "2020-11",
                                                             str_detect(date, "2020-12") ~ "2020-12",
                                                             str_detect(date, "2021-01") ~ "2021-01",
                                                             str_detect(date, "2021-02") ~ "2021-02",
                                                             str_detect(date, "2021-03") ~ "2021-03",
                                                             str_detect(date, "2021-04") ~ "2021-04",
                                                             str_detect(date, "2021-05") ~ "2021-05",
                                                             str_detect(date, "2021-06") ~ "2021-06",
                                                             str_detect(date, "2021-07") ~ "2021-07",
                                                             str_detect(date, "2021-08") ~ "2021-08",
                                                             str_detect(date, "2021-09") ~ "2021-09",
                                                             str_detect(date, "2021-10") ~ "2021-10",
                                                             str_detect(date, "2021-11") ~ "2021-11",
                                                             str_detect(date, "2021-12") ~ "2021-12",
                                                             str_detect(date, "2022-01") ~ "2022-01",
                                                             str_detect(date, "2022-02") ~ "2022-02",
                                                             str_detect(date, "2022-03") ~ "2022-03",
                                                             str_detect(date, "2022-04") ~ "2022-04",
                                                             str_detect(date, "2022-05") ~ "2022-05",
                                                             str_detect(date, "2022-06") ~ "2022-06",
                                                             str_detect(date, "2022-07") ~ "2022-07",
                                                             str_detect(date, "2022-08") ~ "2022-08",
                                                             str_detect(date, "2022-09") ~ "2022-09",
                                                             str_detect(date, "2022-10") ~ "2022-10",
                                                             str_detect(date, "2022-11") ~ "2022-11",
                                                             str_detect(date, "2022-12") ~ "2022-12"))

# Compute the total number of cases per month
monthly_US_case_data <- as.data.frame(aggregate(US_case_data$new_cases, by=list(Date=US_case_data$Date), FUN=sum))

# Manually enter missing data for January and February 2020 using WHO data found online
monthly_US_case_data <- insertRows(monthly_US_case_data, 1, new = c("2020-01", 6))
monthly_US_case_data[2, 2] <- 63

# Rename the columns
colnames(monthly_US_case_data) <- c("Date","case_count")

# Create dataset containing monthly information for all variables to be used in the regression
monthly_data <- merge(nyt_coverage_bls_data, monthly_US_case_data)
monthly_data[,5]<-as.numeric(monthly_data[,5])

percent_change <- c(6)
for(x in 2:35){
  percent_change <- c(percent_change, 100*(monthly_data[x,5]-monthly_data[x-1,5])/(monthly_data[x-1,5]))
}

monthly_data <- monthly_data %>% mutate(Percent_change_cases = percent_change)

# Create the main AnalyticFile which will then be used in the regression
AnalyticFile <- write.csv(monthly_data, "C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\AnalyticFile.csv")
