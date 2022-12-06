#installing and loading packages that help wrangle and visualize data
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('lubridate')
library(tidyverse)
library(ggplot2)
library(lubridate)

#setting current folder to dataset location
setwd("F:/case_study_1_cyclistic")

#loading .csv files
data_21_11 <- read.csv("202111-divvy-tripdata.csv")
data_21_12 <- read.csv("202112-divvy-tripdata.csv")
data_22_01 <- read.csv("202201-divvy-tripdata.csv")
data_22_02 <- read.csv("202202-divvy-tripdata.csv")
data_22_03 <- read.csv("202203-divvy-tripdata.csv")
data_22_04 <- read.csv("202204-divvy-tripdata.csv")
data_22_05 <- read.csv("202205-divvy-tripdata.csv")
data_22_06 <- read.csv("202206-divvy-tripdata.csv")
data_22_07 <- read.csv("202207-divvy-tripdata.csv")
data_22_08 <- read.csv("202208-divvy-tripdata.csv")
data_22_09 <- read.csv("202209-divvy-publictripdata.csv")
data_22_10 <- read.csv("202210-divvy-tripdata.csv")

#combining all months into one dataset
data_year <- bind_rows(data_21_11, data_21_12, data_22_01, data_22_02, data_22_03, data_22_04, data_22_05, data_22_06, data_22_07, data_22_08, data_22_09, data_22_10)

#calculating ride length as seconds
data_year$ride_length_2 <- difftime(as_datetime(data_year$ended_at,format=("%d-%m-%Y %H:%M")), as_datetime(data_year$started_at,format=("%d-%m-%Y %H:%M")))

#converting ride_length_2 as numeric value
data_year$ride_length_2<- as.numeric(as.character(data_year$ride_length_2))

#removing negatives missed in excel and creating a new version of the dataset
data_year_v2<-data_year[!(data_year$ride_length_2<0),]

#inspecting data before analysis
colnames(data_year_v2)
nrow(data_year_v2)
dim(data_year_v2)
head(data_year_v2)
str(data_year_v2)
summary(data_year_v2)

#descriptive analysis for all data
seconds_to_period(mean(data_year_v2$ride_length_2))
seconds_to_period(median(data_year_v2$ride_length_2))
seconds_to_period(max(data_year_v2$ride_length_2))
seconds_to_period(min(data_year_v2$ride_length_2))

#analysis on members vs casual
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual, FUN = mean)
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual, FUN = median)
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual, FUN = max)
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual, FUN = min)

#comparison on daily use
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual + data_year_v2$day_of_week, FUN = mean)
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual + data_year_v2$day_of_week, FUN = median)

#comparison on monthly use
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual + data_year_v2$month, FUN = mean)
aggregate(data_year_v2$ride_length_2 ~ data_year_v2$member_casual + data_year_v2$month, FUN = median)

#rideable types compared
data_year_v2 %>% count(rideable_type, member_casual)

#ridership by day and membership
data_year_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(num_of_rides=n(), average_duration=seconds_to_period(mean(ride_length_2)), median_ride_length=seconds_to_period(median(ride_length_2))) %>%
  arrange(member_casual, weekday)

#ridership by month and membership
data_year_v2 %>%
  group_by(member_casual,month) %>% 
  summarise(num_of_rides=n(), average_duration=seconds_to_period(mean(ride_length_2)), median_ride_length=seconds_to_period(median(ride_length_2))) %>%
  arrange(member_casual, month)

#visual for ridership by day
data_year_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(num_of_rides=n()) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visual for ridership by month
data_year_v2 %>%
  group_by(member_casual,month) %>% 
  summarise(num_of_rides=n()) %>%
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visual for ridership day averages
data_year_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(num_of_rides=n(), average_duration=mean(ride_length_2)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#visual for ridership month averages
data_year_v2 %>%
  group_by(member_casual,month) %>% 
  summarise(num_of_rides=n(), average_duration=mean(ride_length_2)) %>%
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#visual for ridership day medians
data_year_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(num_of_rides=n(), median_ride_length=median(ride_length_2)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = median_ride_length, fill = member_casual)) +
  geom_col(position = "dodge")

#visual for ridership month medians
data_year_v2 %>%
  group_by(member_casual,month) %>% 
  summarise(num_of_rides=n(), median_ride_length=median(ride_length_2)) %>%
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = median_ride_length, fill = member_casual)) +
  geom_col(position = "dodge")

#.csv data for export
daily_avg_med<-data_year_v2 %>%
  mutate(weekday=wday(started_at,label=TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(num_of_rides=n(), average_duration=seconds_to_period(mean(ride_length_2)), median_ride_length=seconds_to_period(median(ride_length_2))) %>%
  arrange(member_casual, weekday)
write.csv(daily_avg_med, file="daily_avg_med.csv")

monthly_avg_med<-data_year_v2 %>%
  group_by(member_casual,month) %>% 
  summarise(num_of_rides=n(), average_duration=seconds_to_period(mean(ride_length_2)), median_ride_length=seconds_to_period(median(ride_length_2))) %>%
  arrange(member_casual, month)
write.csv(monthly_avg_med, file="monthly_avg_med.csv")

rideable_comp<-data_year_v2 %>% count(rideable_type, member_casual)
write.csv(rideable_comp, file="rideable_comp.csv")