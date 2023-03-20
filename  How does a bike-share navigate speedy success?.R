#Install.packages
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
library(ggplot2)
install.packages("scales")
library(scales)
#Uplode datasets 
trip.data1 <- read.csv("202209-divvy-publictripdata.csv")
trip.data2 <- read.csv("202208-divvy-tripdata.csv")
trip.data3 <- read.csv("202207-divvy-tripdata.csv")
trip.data4 <- read.csv("202206-divvy-tripdata.csv")
trip.data5 <- read.csv("202205-divvy-tripdata.csv")
trip.data6 <- read.csv("202204-divvy-tripdata.csv")
trip.data7 <- read.csv("202203-divvy-tripdata.csv")
trip.data8 <- read.csv("202202-divvy-tripdata.csv")
trip.data9 <- read.csv("202201-divvy-tripdata.csv")
trip.data10 <- read.csv("202112-divvy-tripdata.csv")
trip.data11 <- read.csv("202111-divvy-tripdata.csv")
trip.data12 <- read.csv("202110-divvy-tripdata.csv")
#Compare column names each of the files
colnames(trip.data1)
colnames(trip.data2)
colnames(trip.data3)
colnames(trip.data4)
colnames(trip.data5)
colnames(trip.data6)
colnames(trip.data7)
colnames(trip.data8)
colnames(trip.data9)
colnames(trip.data10)
colnames(trip.data11)
colnames(trip.data12)
#Inspect the dataframes and look for incongruencies
str(trip.data1)
str(trip.data2)
str(trip.data3)
str(trip.data4)
str(trip.data5)
str(trip.data6)
str(trip.data7)
str(trip.data8)
str(trip.data9)
str(trip.data10)
str(trip.data11)
str(trip.data12)
#Convert ride_id and rideable_type to character so that they can stack correctly
trip.data1 <- mutate(trip.data1,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data2 <- mutate(trip.data2,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data3 <- mutate(trip.data3,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data4 <- mutate(trip.data4,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data5 <- mutate(trip.data5,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data6 <- mutate(trip.data6,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data7 <- mutate(trip.data7,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data8 <- mutate(trip.data8,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data9 <- mutate(trip.data9,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
tripdata10 <- mutate(trip.data10,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data11<- mutate(trip.data11,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
trip.data12<- mutate(trip.data12,ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
#Stack individual monthly data frames into one big data frame
all_trips <- bind_rows(trip.data1,trip.data2,trip.data3,trip.data4,trip.data5,trip.data6,trip.data7,trip.data8,trip.data9,trip.data10,trip.data11,trip.data12)
#Inspect the new table that has been created
colnames(all_trips) 
nrow(all_trips)  
dim(all_trips)  
head(all_trips) 
str(all_trips)  
summary(all_trips) 
#Conver Data/Time and add columns that list the start hour, end hour,date, month, day, and year of each ride
all_trips$started_at <- lubridate::ymd_hms(all_trips$started_at)
all_trips$ended_at <- lubridate::ymd_hms(all_trips$ended_at)
all_trips$start_hour <- lubridate::hour(all_trips$started_at)
all_trips$end_hour <- lubridate::hour(all_trips$ended_at)
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
#Remove columnns I don't need for analsys
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
#Chacking empty rows for colums start_station_name and end_station_name 
sum(all_trips$start_station_name=="")
sum(all_trips$end_station_name=="")
#Add a "duration" calculation to all_trips (in seconds) and clean dataframe from empty cells "start_station_name" and "end_station_name" and duration less then 1 sec 
all_trips$duration <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips <- all_trips[!(all_trips$start_station_name==""|all_trips$end_station_name==""|all_trips$duration<0), ]
sum(all_trips$duration<1)
all_trips <- all_trips[!(all_trips$duration<1),]
#Add column "duration" in mins
all_trips$duration_mins <- difftime(all_trips$ended_at,all_trips$started_at,units=c("mins"))
format(round(all_trips$duration_mins,2),nsmall=2)
#Compare members and casual users
aggregate(all_trips$duration ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$duration ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$duration ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$duration ~ all_trips$member_casual, FUN = min)
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips$duration ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
#Analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()						
            ,average_duration = mean(duration)) %>% 	
  arrange(member_casual, weekday)							
# Visualziation for number of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma)
# Visualization for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#Visualization for number of rides for each hour
all_trips %>% count(start_hour,sort = T) %>% ggplot() + geom_line(aes(x=start_hour,y = n)) +
  scale_y_continuous(labels = comma)


write.csv(all_trips, "C:\\Users\\nerminbecirovic\\Desktop\\data.csv", row.names=FALSE)


