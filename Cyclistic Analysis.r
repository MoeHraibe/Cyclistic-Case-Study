# Loading packages

library(tidyverse)  # Data cleaning, transformation and manipulation
library(lubridate)
library(hms)
library(dplyr)
library(tidyr)
library(readr)
library(writexl)
library(DescTools)

# Read all past 12 months data

mar_2022_df <- read.csv("202203-divvy-tripdata.csv")
apr_2022_df <- read.csv("202204-divvy-tripdata.csv")
may_2022_df <- read.csv("202205-divvy-tripdata.csv")
jun_2022_df <- read.csv("202206-divvy-tripdata.csv")
jul_2022_df <- read.csv("202207-divvy-tripdata.csv")
aug_2022_df <- read.csv("202208-divvy-tripdata.csv")
sep_2022_df <- read.csv("202209-divvy-publictripdata.csv")
oct_2022_df <- read.csv("202210-divvy-tripdata.csv")
nov_2022_df <- read.csv("202211-divvy-tripdata.csv")
dec_2022_df <- read.csv("202212-divvy-tripdata.csv")
jan_2023_df <- read.csv("202301-divvy-tripdata.csv")
feb_2023_df <- read.csv("202302-divvy-tripdata.csv")

# Merge all data into one data frame

cyclistic_all_data <- rbind(mar_2022_df, apr_2022_df, may_2022_df, jun_2022_df,
jul_2022_df, aug_2022_df, sep_2022_df, oct_2022_df, nov_2022_df,
dec_2022_df, jan_2023_df, feb_2023_df)

# Create a new data frame to work with

cyclistic_df_2 <- cyclistic_all_data

# Calculate ride length by subtracting started_at from ended_at

# First change the data type to accessible data formats

cyclistic_df_2$started_at <- ymd_hms(cyclistic_df_2$started_at)
cyclistic_df_2$ended_at <- ymd_hms(cyclistic_df_2$ended_at)

cyclistic_df_2 <- cyclistic_df_2 %>%
mutate(ride_length = difftime(cyclistic_df_2$ended_at,
cyclistic_df_2$started_at, units = "mins"))

# Round the ride_length

cyclistic_df_2$ride_length <- round(cyclistic_df_2$ride_length, 2)

# Remove any ride length with 0 or negative values

cyclistic_df_2 <- cyclistic_df_2[!(cyclistic_df_2$ride_length <= 0), ]

# Create columns for: day of the week, month, day, year

# Default new starting at date to work with
cyclistic_df_2$date <- as.Date(cyclistic_df_2$started_at)
# Create a column for day of the week
cyclistic_df_2$day_of_week <- format(as.Date(cyclistic_df_2$date), "%A")
# Create a column for month of the year
cyclistic_df_2$month_of_year <- format(as.Date(cyclistic_df_2$date), "%m")
# Create a column for day of the month
cyclistic_df_2$day_of_month <- format(as.Date(cyclistic_df_2$date), "%d")
# Create a column for year
cyclistic_df_2$year <- format(as.Date(cyclistic_df_2$date), "%Y")
# Create a column for time of the day
cyclistic_df_2$time <- format(cyclistic_df_2$started_at, "%H:%M:%S")
# Create a column for the hour of the day
cyclistic_df_2$time <- as_hms(cyclistic_df_2$time)
cyclistic_df_2$hour <- hour(cyclistic_df_2$time)

# Create a column for differnet seasons

cyclistic_df_2 <- cyclistic_df_2  %>%
mutate(season = case_when(month_of_year == "03" ~ "Spring",
month_of_year == "04" ~ "Spring",
month_of_year == "05" ~ "Spring",
month_of_year == "06" ~ "Summer",
month_of_year == "07" ~ "Summer",
month_of_year == "08" ~ "Summer",
month_of_year == "09" ~ "Fall",
month_of_year == "10" ~ "Fall",
month_of_year == "11" ~ "Fall",
month_of_year == "12" ~ "Winter",
month_of_year == "01" ~ "Winter",
month_of_year == "02" ~ "Winter"))

# Create a column for the time of the day

cyclistic_df_2 <- cyclistic_df_2  %>%
mutate(time_of_day = case_when(hour == "0" ~ "Night",
hour == "1" ~ "Night",
hour == "2" ~ "Night",
hour == "3" ~ "Night",
hour == "4" ~ "Night",
hour == "5" ~ "Night",
hour == "6" ~ "Morning",
hour == "7" ~ "Morning",
hour == "8" ~ "Morning",
hour == "9" ~ "Morning",
hour == "10" ~ "Morning",
hour == "11" ~ "Morning",
hour == "12" ~ "Afternoon",
hour == "13" ~ "Afternoon",
hour == "14" ~ "Afternoon",
hour == "15" ~ "Afternoon",
hour == "16" ~ "Afternoon",
hour == "17" ~ "Afternoon",
hour == "18" ~ "Evening",
hour == "19" ~ "Evening",
hour == "20" ~ "Evening",
hour == "21" ~ "Evening",
hour == "22" ~ "Evening",
hour == "23" ~ "Evening"))

# Remove unneeded columns

cyclistic_df_2 <- cyclistic_df_2  %>%
select(-c(ride_id, start_station_id, end_station_id,
start_lat, end_lat, start_lng, end_lng))

#--------------------Total rides------------------------

# Total number of rides
nrow(cyclistic_df_2)

#-------------------Member type-------------------------

# Number of each member type
cyclistic_df_2  %>%
group_by(member_casual)  %>%
count(member_casual)

#-------------------Type of bike-------------------------

# Total rides by rideable types
cyclistic_df_2  %>%
group_by(rideable_type) %>%
count(rideable_type)

# Total rides by rideable type and member type
cyclistic_df_2  %>%
group_by(member_casual, rideable_type) %>%
count(member_casual)

#------------------Hours------------------------------
# Total number rides at each hour
cyclistic_df_2  %>%
group_by(hour)  %>%
count(hour) %>%
print(n = 24)

# Total number of rides by member type and at each hour
cyclistic_df_2  %>%
group_by(member_casual) %>%
count(hour)  %>%
print(n = 48)

#-------------------Time of day--------------------

cyclistic_df_2 %>%
group_by(member_casual, time_of_day) %>%
count(hour) %>%
print(n = 48)

#------------------Day of week-------------------

# Total number of rides by day of the week
cyclistic_df_2  %>%
count(day_of_week)

# Total number of rides by member type and by day of the week
cyclistic_df_2  %>%
group_by(member_casual) %>%
count(day_of_week)

#---------------Day of the month----------------------

# Total number of rides by day of the month
cyclistic_df_2  %>%
count(day_of_month)

# Total number of rides by member type and by day of the month
group_by(member_casual) %>%
count(day_of_month) %>%
print(n = 62)
cyclistic_df_2  %>%

#-------------------Month------------------------------

# Total number of rides per month
cyclistic_df_2  %>%
count(month_of_year)

# Total number of rides by member type and per month
cyclistic_df_2  %>%
group_by(member_casual) %>%
count(month_of_year) %>%
print(n = 24)

#-----------------Seasons-------------------------

# Total number of rides per season
cyclistic_df_2  %>%
count(season)

# Total number of rides by member type and per season
cyclistic_df_2  %>%
group_by(member_casual, season) %>%
count(season)

#--------------------Months and Seasons------------------

cyclistic_df_2 %>%
group_by(member_casual, season) %>%
count(month_of_year) %>%
print(n = 24)

#-------------------Average ride length-----------------

# Average ride length of all rides
average_ride_length <- mean(cyclistic_df_2$ride_length)
print(average_ride_length)

#-----------------------Member type--------------------

# Average ride length by member type
cyclistic_df_2  %>% group_by(member_casual) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

#---------------------Ride types-----------------
# Average ride length by ride types
cyclistic_df_2  %>% group_by(rideable_type) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

# Average ride length by member type and ride type
cyclistic_df_2  %>% group_by(member_casual, rideable_type) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

#-------------------Hour-----------------

# Average ride length at every hour of the day
cyclistic_df_2  %>% group_by(hour) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 24)

# Average ride length by member type and at every hour of the day
cyclistic_df_2  %>% group_by(member_casual, hour) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 48)

#----------------------------Time of day-------------------------

# Average ride length by time of day
cyclistic_df_2  %>% group_by(time_of_day) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

# Average ride length by time of day and member type
cyclistic_df_2  %>% group_by(member_casual, time_of_day) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

#-----------------------Day of week-------------------

# Average ride length by day of the week
cyclistic_df_2  %>% group_by(day_of_week) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

# Average ride length by member type and day of the week
cyclistic_df_2  %>% group_by(member_casual, day_of_week) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

#----------------------Day of month--------------------

# Average ride length by day of month
cyclistic_df_2  %>% group_by(day_of_month) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 31)

# Average ride length by member type and day of month
cyclistic_df_2  %>% group_by(member_casual, day_of_month) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 62)

#-------------------------Month-------------------------

# Average ride length by month
cyclistic_df_2  %>% group_by(month_of_year) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 12)

# Average ride length by month and member type
cyclistic_df_2  %>% group_by(member_casual, month_of_year) %>%
summarise(average_ride_length = round(mean(ride_length), 2)) %>%
print(n = 24)

#------------------------Season-------------------------

# Average ride length by season
cyclistic_df_2  %>% group_by(season) %>%
summarise(average_ride_length = round(mean(ride_length), 2))

# Average ride length by season and member type
cyclistic_df_2  %>% group_by(member_casual, season) %>%
summarise(average_ride_length = round(mean(ride_length), 2))
