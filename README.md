
# Cyclistic-Case-Study

## **Introduction**
---
### **Scenario**
As a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago, I am tasked alongside my team to understand how casual riders differ from annual members. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve our recommendations.

### **About the Company: Cyclistic**
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are tracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system at anytime. Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships

### **Scope of Work**

|Phases of the Project|Deliverables|Tasks|
|---|---|---|
|Ask|1.	Discuss the project|<ul><li>Clearly state the business task</li><li>Define project goals and consider stakeholders’expectations</li></ul>|
|Prepare|2. Prepare data for exploration|<ul><li>Address licensing, privacy, security, and accessibility issues.</li><li>Confirm data integrity</li><li>Identify how the data will help us answer our question</li><li>Check for any problems with the data</li><li>Sort and filter the data</li></ul>|
|Process|3. Process data for analysis|<ul><li>Apply the right data tools for processing and analysis</li><li>Clean the data to ensure the data integrity</li></ul>|
|Analyze|4. Conduct a comprehensive analysis|<ul><li>Aggregate data and perform calculations in order to identify trends and relationships
|Share|5. Share key findings with stakeholders|<ul><li>Create compelling visualizations and an effective presentation that answer the original business problem|
|Act|6.	Act on key findings|<ul><li>Provide recommendations to the executive team based on the conducted analysis.|


## **Phase 1: Ask**
---
#### **Discuss the project**
##### **Business task:**

Analyze how causal riders differ from annual members and then provide data-driven recommendations on how to convert casual riders to annual members.


##### **Stakeholders and their expectations:**

|Stakeholders|Expectations|Goals|
|---|---|---|
|Cyclistic Executive Team|Receive clear, compelling, and straightforward insights and visualizations that will inform their data-driven decisions towards the new marketing strategy|Approve or decline the recommendations provided by the marketing analytics team.|
|Lily Moreno, Director of Marketing|Receive evidence that will either prove her theory right or wrong|Convert casual riders to annual members therefore increasing the company’s annual profits|
|Cyclistic Marketing Analytics Team|Reveal differences between causal riders and annual members|Provide data-driven recommendations on how to convert casual riders to annual members|


## **Phase 2: Prepare**
---
#### **Prepare data for exploration**

The data for this case study is provided by Motivate International Inc. via this [link](https://divvy-tripdata.s3.amazonaws.com/index.html). I’ll be using the last 12 months of Cyclistic data for my analysis, which covers the period from March 2022 to February 2023.

##### **Data Source:**

The data is public and has been made available by Motivate International Inc. to anyone via this [link](https://divvy-tripdata.s3.amazonaws.com/index.html) under this [license](https://ride.divvybikes.com/data-license-agreement).

Note that the data has a different name because Cyclistic is a fictional company.

##### **Data Organization:**

For my analysis, I’ll be using Cyclistic’s data from the last 12 months, which covers the period from March 2022 to February 2023. The data is organized in months; where each month’s data is a separate comma-separated value file. Each CSV file consists of 13 columns and thousands of rows. Each row represents a unique bike trip under the bike-share program with its different features. The features consist of a unique hashed ride Id number which serves as the primary key of the table, the type of the bike used (classic, docked, electric), the date and time each ride starts and ends, the name and Id of the starting and ending station, the type of customer (casual rider or annual member), and the starting and ending latitude and longitude of each ride.

##### **Data Quality:**
I used the ROCCC approach to determine the quality of the data.

* **R**eliable: 
    
    Complete:  The data has some missing values in some columns that were removed to make sure the analysis is accurate and is not skewed by missing values.
    
    Accurate: Based on our initial investigation, most of the rows are accurate and free from error.
    
    Duplicates: There are several duplicates in the rider Id but this is not an issue because each instance is unique and represent a different ride.
    Therefore, the data can be relied on.
* **O**riginal:
    
    The data is first-party data and it is collected by the City of Chicago using its own resources and was made available to the public.
* **C**omprehensive:
    
    The provided datasets contain all the needed information that allows us to conduct our analysis. 
* **C**urrent: 
    
    The data we are using is up-to-date (as of the time this study was conducted).
* **C**ited:
    
    The data belongs to the City of Chicago under this [license](https://ride.divvybikes.com/data-license-agreement).

##### **Ethical Concerns:**

* Licensing:
        
    The data is protected under this [license agreement](https://ride.divvybikes.com/data-license-agreement).
* Privacy:
    
    The data is free from any personal identifiable or sensitive information.

    Sufficient anonymization is applied to the data subjects.   
* Accessibility:
    
    The data is public and is made available to anyone.

##### **Data Purpose:**
Discover patterns and useful insights on how casual riders and annual members differ.

## **Phase 3: Process**
---
#### **Process data for analysis**

1. Tools:

    I’m using R programming as my cleaning and analysis tool for its ability to handle and manipulate huge datasets efficiently. And Visual Studio Code as my IDE (Integrated development environment).

2. Setting up my environment:

```{r}
library(tidyverse)  # Data cleaning, transformation, and manipulation
library(lubridate)
library(hms)
library(dplyr)
library(tidyr)
library(readr)
library(writexl)
library(DescTools)
```
3. Reading and combining all 12 datasets

```{r}
# Read all past 12 months' data

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
```
4. Converting data to the right data type

```{r}
cyclistic_df_2$started_at <- ymd_hms(cyclistic_df_2$started_at)
cyclistic_df_2$ended_at <- ymd_hms(cyclistic_df_2$ended_at)
```
5. Preparing data for analysis:

    * Created new columns to help in my analysis

```{r}
# Calculate ride length by subtracting started_at from ended_at

cyclistic_df_2 <- cyclistic_df_2 %>%
mutate(ride_length = difftime(cyclistic_df_2$ended_at,
cyclistic_df_2$started_at, units = "mins"))

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

# Create a column for different seasons

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
```

* Removed unneeded columns

```{r}
cyclistic_df_2 <- cyclistic_df_2  %>%
select(-c(ride_id, start_station_id, end_station_id,
start_lat, end_lat, start_lng, end_lng))
```

6. Checking data for errors

    Removed any row with a ride length that is less or equal to zero because it is illogical.

```{r}
cyclistic_df_2 <- cyclistic_df_2[!(cyclistic_df_2$ride_length <= 0), ]
```
7. Checking for duplicates and removing NA values
```{r}
# Remove rows with NA values

cyclistic_df_2 <- na.omit(cyclistic_df_2)

# Remove duplicate rows

cyclistic_df_2 <- cyclistic_df_2[!duplicated(cyclistic_df_2), ]
```

8. Taking a quick view of my data before starting my analysis

```{r}
glimpse(cyclistic_df_2)
```
## **Phase 4: Analyze**
---
#### **Conduct a comprehensive analysis**

* Click [here](https://github.com/MoeHraibe/Cyclistic-Case-Study/blob/master/02.%20Analysis/Cyclistic%20Analysis.r) to view the R script and the summary of the analysis.

## **Phase 5: Share**
---
#### **Share key findings with stakeholders**

Data visualizations and dashboard:

I created my visualizations and dashboard using Tableau.

* Click [here](https://public.tableau.com/app/profile/mohamad.ali.hraibe/viz/Cyclistic-Case-Study/Cyclistic-Case-Study) to view them.

## **Phase 6: Act**
---
#### **Act on key findings**

After conducting my analysis, I reached the following conclusions:

**Both Users:**
    
* Both types of riders have the highest number of rides in the afternoon.
* Both types of riders have the highest number of rides during the summer.

**Casuals:**
    
* Take more rides during weekends
* Take longer rides on weekends
* Consistently have longer ride lengths than members 
* They use docked bikes unlike members
* Big disparity between the average ride length of docked bikes and other bikes (docked bike average ride length = 49.68 minutes vs bike with the second longest average ride length = 24.38 minutes)

**Members:**

* Take more rides during weekdays
* Have a steady average ride length throughout the week

Here are my top 3 recommendations:

1. Design riding packages specific for docked bikes, with lower prices after particular mileage or duration.

2. Introduces new promotions that target casual riders at their busiest times:

    o	Time: Afternoon

    o	Day: Weekends

3. Design seasonal packages that give more flexibility for casual riders and encourage them to get a membership for a specific period in case they are not willing to pay for an annual subscription.
---
---
