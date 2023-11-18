#Anisa Tse and Anna Pham

library(stringr)
library(dplyr)
library(testthat)

comm_health_df <- read.csv("Community_Health_Indicator_Reports__CHIRS___Trend_Data.csv") #loads data from the community health dataframe
air_qual_df <- read.csv("Air_Quality.csv") #loads data from the air quality dataframe

