#The following code details my data science project on the 
#Uber Pickups in New York City Dataset. It is a data visualization project that 
#uses the GGPLOT2 library in R to understand the data and develop an intuition 
#for understanding the types of customers who avail trips.

#Data storytelling is a methodology for communicating information, tailored to 
#a specific audience, with a compelling narrative. Through visualization, 
#companies can avail the benefits of understanding the complex data and gain
#insights that would help them craft better decisions.

#------------------------------------------------------------------------------#

#Importing packages
library(ggplot2)

install.packages("ggthemes")
library(ggthemes)

library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(ggsci)


#Importing Data

#We have separate csv files for each month's data from April to September. 
april <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-apr14.csv")
may <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-may14.csv")
june <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-jun14.csv")
july <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-jul14.csv")
august <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-aug14.csv")
september <- read.csv("C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/uber-raw-data-sep14.csv")

#We can rbind these monthly datasets to get one dataframe
data <- rbind(april, may, june, july, august, september)

#Now we apply some formatting to the Date Time column, and 
#create factors of time objects

data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"),
                    format = "%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)

data$day <- factor(day(data$Date.Time))
data $month <- factor(month(data$Date.Time, label = TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label = TRUE))

data$hour <- factor(hour(hms(data$Time)))
data$minute <- factor(minute(hms(data$Time)))
data$second <- factor(second(hms(data$Time)))

#write.csv(data, "C:/Users/angad/Desktop/ANGAD/Projects/Uber Data Analysis/Uber-dataset/data2014.csv")

#Plotting the trips by the hours in a day

hourly_data <- data %>%
  group_by(hour) %>%
  summarize(Total_Trips = sum(n()))

datatable(hourly_data)

ggplot(hourly_data, aes(x = hour, y = Total_Trips)) +
  geom_col(fill = "seagreen", color = 'black') +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#We can also aggregate this hourly data monthwise
monthly_hourly_data <- data %>%
  group_by(month, hour) %>%
  summarize(Total_Trips = sum(n()))

ggplot(monthly_hourly_data, aes(x = hour, y = Total_Trips, fill = month)) +
  geom_col() + 
  ggtitle("Trips by hour, monthly") + 
  scale_fill_jco()+
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)


#Plotting the trips by days of a month
daily_data <- data %>%
  group_by(day) %>%
  summarize(Total_Trips = sum(n()))

datatable(daily_data)

ggplot(daily_data, aes(x = day, y = Total_Trips)) +
  geom_col(fill = "seagreen", color = 'black') +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


#Day of week and Month
dayofweek_data <- data %>%
  group_by(dayofweek) %>%
  summarize(Total_Trips = sum(n()))

datatable(dayofweek_data)

ggplot(dayofweek_data, aes(x = dayofweek, y = Total_Trips)) +
  geom_col(fill = "seagreen", color = 'black') +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Group the dayofweek data by month
dayofweek_month_data <- data %>%
  group_by(month, dayofweek) %>%
  summarize(Total_Trips = sum(n()))

ggplot(dayofweek_month_data, aes(x = dayofweek, y = Total_Trips, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Trips by day, month-wise") +
  scale_fill_jco()+
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)


#Plotting trips by month
monthly_data <- data %>%
  group_by(month) %>%
  summarize(Total_Trips = sum(n()))

datatable(monthly_data)

ggplot(monthly_data, aes(x = month, y = Total_Trips, fill = month)) +
  geom_col() +
  ggtitle("Trips by Month") +
  scale_fill_jco()+
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)

#Plotting trips by Bases
bases_data <- data %>%
  group_by(Base) %>%
  summarize(Total_Trips = sum(n()))

datatable(bases_data)

ggplot(bases_data, aes(x = Base, y = Total_Trips)) +
  geom_col(fill = "seagreen", color = "black") +
  ggtitle("Trips by Base") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)

ggplot(data, aes(x = Base, fill = month)) +
  geom_bar(position = "dodge") + 
  ggtitle("Trips by Base, month-wise") +
  scale_fill_jco() +
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma) 

ggplot(data, aes(x = Base, fill = dayofweek)) +
  geom_bar(position = "dodge") + 
  ggtitle("Trips by Base, day-wise") +
  scale_fill_jco() +
  theme_classic() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)


#Creating heatmap visualizations for day, hour, and month

#Day and Hour
daily_hourly_data <- data %>%
  group_by(day, hour) %>%
  summarize(Total_Trips = sum(n()))

datatable(daily_hourly_data)

ggplot(daily_hourly_data, aes(x = day, y = hour, fill = Total_Trips)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Day and Hour")


#Day and Month
day_month_data <- data %>%
  group_by(month, day) %>%
  summarize(Total_Trips = sum(n()))

ggplot(day_month_data, aes(x = day, y = month, fill = Total_Trips)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Day and Month")

ggplot(dayofweek_month_data, aes(x = dayofweek, y = month, fill = Total_Trips)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap by Day of Week and Month")


#Bases with month and day
monthly_base_data <- data %>%
  group_by(Base, month) %>%
  summarize(Total_Trips = sum(n()))

ggplot(monthly_base_data, aes(x = Base, y = month, fill = Total_Trips)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap for Bases by Month")


daily_base_data <- data %>%
  group_by(Base, dayofweek) %>%
  summarize(Total_Trips = sum(n()))

ggplot(daily_base_data, aes(x = Base, y = dayofweek, fill = Total_Trips)) +
  geom_tile(color = "white") +
  ggtitle("Heatmap for Bases by Day of week")


#We can visualise the Uber Pickups using a map created on GGPLOT2
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "seagreen") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC Map for Uber Rides during April 2014-September 2014")

ggplot(data, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC Map for Uber Rides during April 2014-September 2014, Base-wise")

