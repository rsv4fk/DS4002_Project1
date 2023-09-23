# import libraries
library(tidyverse)
library(dplyr)

# data cleaning
rawdata <- read.csv("C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\RawAnalyticFile.csv")
data1 <- rawdata[,-c(1, 4, 5, 7, 8, 9, 13, 14, 16, 17, 18, 21)]
data2 <- data1 %>% filter(document_type == "article")
data3 <- data2[,-6]

data4 <- as.data.frame(data3)

for(x in 1:10){
  data4[,x] <- tolower(data4[,x])
}

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

# calculating counts of COVID mentions per month

for(x in 1:4199){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_01 <- count_2020_01 + 1
  }else{
    count_2020_01 <- count_2020_01 + 0}
}

for(x in 4200:8145){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_02 <- count_2020_02 + 1
  }else{
    count_2020_02 <- count_2020_02 + 0}
}

for(x in 8146:12803){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_03 <- count_2020_03 + 1
  }else{
    count_2020_03 <- count_2020_03 + 0}
}

for(x in 12804:17556){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_04 <- count_2020_04 + 1
  }else{
    count_2020_04 <- count_2020_04 + 0}
}

for(x in 17556:21697){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_05 <- count_2020_05 + 1
  }else{
    count_2020_05 <- count_2020_05 + 0}
}


for(x in 21698:25778){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_06 <- count_2020_06 + 1
  }else{
    count_2020_06 <- count_2020_06 + 0}
}

for(x in 25779:29905){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_07 <- count_2020_07 + 1
  }else{
    count_2020_07 <- count_2020_07 + 0}
}

for(x in 29906:33961){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_08 <- count_2020_08 + 1
  }else{
    count_2020_08 <- count_2020_08 + 0}
}

for(x in 33962:38250){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_09 <- count_2020_09 + 1
  }else{
    count_2020_09 <- count_2020_09 + 0}
}

for(x in 38251:42994){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_10 <- count_2020_10 + 1
  }else{
    count_2020_10 <- count_2020_10 + 0}
}

for(x in 42995:47125){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_11 <- count_2020_11 + 1
  }else{
    count_2020_11 <- count_2020_11 + 0}
}


for(x in 47126:50993){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2020_12 <- count_2020_12 + 1
  }else{
    count_2020_12 <- count_2020_12 + 0}
}

for(x in 50994:55091){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_01 <- count_2021_01 + 1
  }else{
    count_2021_01 <- count_2021_01 + 0}
}

for(x in 55092:58941){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_02 <- count_2021_02 + 1
  }else{
    count_2021_02 <- count_2021_02 + 0}
}

for(x in 58942:63234){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_03 <- count_2021_03 + 1
  }else{
    count_2021_03 <- count_2021_03 + 0}
}


for(x in 63235:67354){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_04 <- count_2021_04 + 1
  }else{
    count_2021_04 <- count_2021_04 + 0}
}

for(x in 67355:71314){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_05 <- count_2021_05 + 1
  }else{
    count_2021_05 <- count_2021_05 + 0}
}

for(x in 71315:75196){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_06 <- count_2021_06 + 1
  }else{
    count_2021_06 <- count_2021_06 + 0}
}

for(x in 75197:79185){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_07 <- count_2021_07 + 1
  }else{
    count_2021_07 <- count_2021_07 + 0}
}


for(x in 79186:82957){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_08 <- count_2021_08 + 1
  }else{
    count_2021_08 <- count_2021_08 + 0}
}


for(x in 82958:86884){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_09 <- count_2021_09 + 1
  }else{
    count_2021_09 <- count_2021_09 + 0}
}

for(x in 86885:90862){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_10 <- count_2021_10 + 1
  }else{
    count_2021_10 <- count_2021_10 + 0}
}

for(x in 90863:94600){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_11 <- count_2021_11 + 1
  }else{
    count_2021_11 <- count_2021_11 + 0}
}

for(x in 94601:98217){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2021_12 <- count_2021_12 + 1
  }else{
    count_2021_12 <- count_2021_12 + 0}
}

for(x in 98218:101780){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_01 <- count_2022_01 + 1
  }else{
    count_2022_01 <- count_2022_01 + 0}
}

for(x in 101781:105595){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_02 <- count_2022_02 + 1
  }else{
    count_2022_02 <- count_2022_02 + 0}
}


for(x in 105596:109600){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_03 <- count_2022_03 + 1
  }else{
    count_2022_03 <- count_2022_03 + 0}
}

for(x in 109601:113333){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_04 <- count_2022_04 + 1
  }else{
    count_2022_04 <- count_2022_04 + 0}
}

for(x in 113334:117083){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_05 <- count_2022_05 + 1
  }else{
    count_2022_05 <- count_2022_05 + 0}
}

for(x in 117084:120943){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_06 <- count_2022_06 + 1
  }else{
    count_2022_06 <- count_2022_06 + 0}
}


for(x in 120944:124504){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_07 <- count_2022_07 + 1
  }else{
    count_2022_07 <- count_2022_07 + 0}
}


for(x in 124505:127996){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_08 <- count_2022_08 + 1
  }else{
    count_2022_08 <- count_2022_08 + 0}
}

for(x in 127997:131770){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_09 <- count_2022_09 + 1
  }else{
    count_2022_09 <- count_2022_09 + 0}
}

for(x in 131771:135411){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_10 <- count_2022_10 + 1
  }else{
    count_2022_10 <- count_2022_10 + 0}
}

for(x in 135412:137061){
  if(grepl("virus", data4[x, 2]) | 
     grepl("virus", data4[x, 3]) |
     grepl("virus", data4[x, 4]) |
     grepl("virus", data4[x, 7]) |
     grepl("virus", data4[x, 8]) |
     grepl("corona", data4[x, 2]) |
     grepl("corona", data4[x, 3]) |
     grepl("corona", data4[x, 4]) |
     grepl("corona", data4[x, 7]) |
     grepl("corona", data4[x, 8]) |
     grepl("covid", data4[x, 2]) |
     grepl("covid", data4[x, 3]) |
     grepl("covid", data4[x, 4]) |
     grepl("covid", data4[x, 7]) |
     grepl("covid", data4[x, 8]) |
     grepl("pandemic", data4[x, 2]) |
     grepl("pandemic", data4[x, 3]) |
     grepl("pandemic", data4[x, 4]) |
     grepl("pandemic", data4[x, 7]) |
     grepl("pandemic", data4[x, 8]) |
     grepl("epidemic", data4[x, 2]) |
     grepl("epidemic", data4[x, 3]) |
     grepl("epidemic", data4[x, 4]) |
     grepl("epidemic", data4[x, 7]) |
     grepl("epidemic", data4[x, 8])){
    count_2022_11 <- count_2022_11 + 1
  }else{
    count_2022_11 <- count_2022_11 + 0}
}

# calculating COVID coverage as percentage of all articles published in a given month

monthly_totals <- as.data.frame(data4 %>% count(Date))[,2]
count_totals <- c(count_2020_01, count_2020_02, count_2020_03, count_2020_04, count_2020_05, count_2020_06, count_2020_07, count_2020_08, count_2020_09, count_2020_10, count_2020_11, count_2020_12, count_2021_01, count_2021_02, count_2021_03, count_2021_04, count_2021_05, count_2021_06, count_2021_07, count_2021_08, count_2021_09, count_2021_10, count_2021_11, count_2021_12, count_2022_01, count_2022_02, count_2022_03, count_2022_04, count_2022_05, count_2022_06, count_2022_07, count_2022_08, count_2022_09, count_2022_10, count_2022_11)
coverage_freq <- 100*(count_totals/monthly_totals)

data5 <- as.data.frame(coverage_freq) %>% mutate(Date = c("2020-01",
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

data6 <- merge(data5, data4)
MainAnalyticFile <- write.csv(data6, "C:\\Users\\ravza\\OneDrive\\Documents\\3rd Year\\Fall 2023\\DS 4002\\MainAnalyticFile.csv")
