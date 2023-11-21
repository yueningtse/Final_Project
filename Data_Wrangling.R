#Anisa Tse and Anna Pham

#import library packages
library(stringr)
library(dplyr)
library(testthat)
library(DescTools)


comm_health_df_raw <- read.csv("Community_Health_Indicator_Reports__CHIRS___Trend_Data.csv") #loads data from the community health dataframe
air_qual_df_raw <- read.csv("Air_Quality.csv") #loads data from the air quality dataframe

#Creating a dataset with rows where Name = Fine Particulate Matter (PM2.5), Geo Type Name = Borough
#and Time Period = Annual Average
air_qual_df <- filter(air_qual_df_raw, Geo.Type.Name == "Borough")

air_qual_df <- filter(air_qual_df, Name == "Fine Particulate Matter (PM2.5)" | 
                        Name == "Nitrogen Dioxide (NO2)" | 
                        Name == "Ozone (O3)" | 
                        Name == "Sulfur Dioxide (SO2)")


#Create a new column in the air_qual_df that stores the county names 
i <- 1
for(i in 1:length(air_qual_df$Geo.Place.Name)){
  if(str_detect(air_qual_df$Geo.Place.Name[i], "Bronx")){
    air_qual_df$County[i] <- "Bronx"
  } else if(str_detect(air_qual_df$Geo.Place.Name[i], "Brooklyn")){
    air_qual_df$County[i] <- "Kings"
  } else if(str_detect(air_qual_df$Geo.Place.Name[i], "Manhattan")){
    air_qual_df$County[i] <- "New York"
  } else if(str_detect(air_qual_df$Geo.Place.Name[i], "Queens")){
    air_qual_df$County[i] <- "Queens"
  } else if(str_detect(air_qual_df$Geo.Place.Name[i], "Staten Island")){
    air_qual_df$County[i] <- "Richmond"
  } else{
    air_qual_df$County[i] <- NA
  }
}


#Create a new column in the air_qual_df that stores the years
i <- 1
for(i in 1:length(air_qual_df$Start_Date)){
  length <- nchar(air_qual_df$Start_Date[i])
  last4 <- as.numeric((substr(air_qual_df$Start_Date[i], length - 3, length)))
 if(substr(air_qual_df$Start_Date[i], 1, 2) == "12"){
   air_qual_df$Year[i] <- as.character(last4 + 1)
 } else{
   air_qual_df$Year[i] <- as.character(last4)
 }
}


#Filtering the raw community health data frame by the Bronx, Kings, New York, Queens, and Richmond counties
#and creating a new data frame
comm_health_df <- filter(comm_health_df_raw, County.Name == "Bronx" | 
                           County.Name == "Kings" | 
                           County.Name == "New York" | 
                           County.Name == "Queens" | 
                           County.Name == "Richmond")

                         
#Filtering the observations on asthma in the comm_health_dataset
comm_health_df <- filter(comm_health_df, Indicator.Name %like% "%asthma%" | Indicator.Name %like% "%Asthma%")


#Joining the air_qual_df and comm_health_df by county name and year
df <- inner_join(air_qual_df, comm_health_df, by = c("County" = "County.Name", "Year" = "Date.Year"), relationship = "many-to-many")


#Create new categorical variable column in the df data frame for mortality and hospitalization categories
i <- 1
for(i in 1:length(df$Indicator.Name)){
  if(str_detect(df$Indicator.Name[i], "hospitalization")){
    df$Category[i] <- "Hospitalization"
  } else if(str_detect(df$Indicator.Name[i], "mortality")){
    df$Category[i] <- "Mortality"
  } else{
    df$Category[i] <- NA
  }
}

#Create a new dataframe where Category is hospitalization
hosp_df <- filter(df, Category == "Hospitalization")


#Calculate the quartile values
quartile <- quantile(hosp_df$Trend.Data.County.Value, probs = c(0.25, 0.5, 0.75, 1))


#Categorize hospitalization trend data values into risk levels 
i <- 1
for(i in 1:length(df$Trend.Data.County.Value)){
  if(df$Category[i] == "Hospitalization" && df$Trend.Data.County.Value[i] < quartile[1]){
    df$Risk_Level[i] <- "Low"
  } else if(df$Category[i] == "Hospitalization" && df$Trend.Data.County.Value[i] < quartile[2]){
    df$Risk_Level[i] <- "Medium"
  } else if(df$Category[i] == "Hospitalization" && df$Trend.Data.County.Value[i] < quartile[3]){
    df$Risk_Level[i] <- "High"
  } else if(df$Category[i] == "Hospitalization" && df$Trend.Data.County.Value[i] < quartile[4]){
    df$Risk_Level[i] <- "Very High"
  } else{
    df$Risk_Level[i] <- NA
  }
}


#Create a new column containing the percent change between the County trend data values
#and NYC trend data values
i <- 1
for(i in 1:length(df$Trend.Data.County.Value)){
  diff <- df$Trend.Data.County.Value[i] - df$Trend.Data.NYC.Value[i]
  perc_diff <- round((diff / df$Trend.Data.NYC.Value[i]) * 100, digits = 0)
  df$county_vs_nyc[i] <- perc_diff
}


#summarizing the dataframe, and providing a mean value for different indicator name values and year
grouped_df <- group_by(df, Indicator.Name, Year)
summary_hosp_df <- summarize(grouped_df, mean_value = mean(Trend.Data.County.Value, na.rm = TRUE))


