library(dplyr)
library(stringr)

df <- read.csv("Air_Quality_and_Community_Health_Dataset.csv")
df$YearChar <- as.character(df$Year)

df <- filter(df, Health.Topic == "Respiratory Disease Indicators")

df1 <- filter(df, Indicator.Name == "Asthma hospitalization rate per 10,000")

i <- 1
for(i in 1:length(df1$Time.Period)){
  if(!is.na(df1$Time.Period[i]) && str_detect(df1$Time.Period[i], "Summer")){
    df1$Season[i] <- "Summer"
  } else if(!is.na(df1$Time.Period[i]) && str_detect(df1$Time.Period[i], "Winter")){
    df1$Season[i] <- "Winter"
  } else{
    df1$Season[i] <- "Annual Average"
  }
}

df2_1 <- subset(df, grepl("Asthma hospitalization rate per 10,000", Indicator.Name) & 
                Name == "Fine Particulate Matter (PM2.5)" & 
                grepl("Annual Average", Time.Period))

df2_1$Year <- factor(df2_1$Year, levels = sort(unique(df2_1$Year)),
                  labels = sort(unique(df2_1$Year)))


df2_2 <- subset(df, grepl("Asthma hospitalization rate per 10,000", Indicator.Name) & 
                Name == "Fine Particulate Matter (PM2.5)" & 
                grepl("Annual Average", Time.Period))


df3_1 <- subset(df, grepl("Asthma hospitalization rate per 10,000", Indicator.Name))

df3_1$Year <- factor(df3_1$Year, levels = sort(unique(df3_1$Year)),
                   labels = sort(unique(df3_1$Year)))

i <- 1
for(i in 1:length(df3_1$Time.Period)){
  if(!is.na(df3_1$Time.Period[i]) && str_detect(df3_1$Time.Period[i], "Summer")){
    df3_1$Season[i] <- "Summer"
  } else if(!is.na(df3_1$Time.Period[i]) && str_detect(df3_1$Time.Period[i], "Winter")){
    df3_1$Season[i] <- "Winter"
  } else{
    df3_1$Season[i] <- "Annual Average"
  }
}


df3_2 <- subset(df, grepl("Asthma hospitalization rate per 10,000", Indicator.Name))

df3_2$Year <- factor(df3_2$Year, levels = sort(unique(df3_2$Year)),
                     labels = sort(unique(df3_2$Year)))
i <- 1
for(i in 1:length(df3_2$Time.Period)){
  if(!is.na(df3_2$Time.Period[i]) && str_detect(df3_2$Time.Period[i], "Summer")){
    df3_2$Season[i] <- "Summer"
  } else if(!is.na(df3_2$Time.Period[i]) && str_detect(df3_2$Time.Period[i], "Winter")){
    df3_2$Season[i] <- "Winter"
  } else{
    df3_2$Season[i] <- "Annual Average"
  }
}





