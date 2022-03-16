# !diagnostics suppress=<comma-separated list of variables>
library("tidyverse")
library("ggplot2")
library("lubridate")
library("tidygeocoder")
library("geosphere")
library("gridExtra") 
library("ggmap") 
library("knitr")
library("janitor")
library("dplyr")


#importing data
X202102_divvy_tripdata <- read_csv("Documents/bike_share_projects/202102-divvy-tripdata.csv")
X202103_divvy_tripdata <- read_csv("Documents/bike_share_projects/202103-divvy-tripdata.csv")
X202104_divvy_tripdata <- read_csv("Documents/bike_share_projects/202104-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("Documents/bike_share_projects/202105-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("Documents/bike_share_projects/202106-divvy-tripdata.csv")
X202107_divvy_tripdata <- read_csv("Documents/bike_share_projects/202107-divvy-tripdata.csv")
X202108_divvy_tripdata <- read_csv("Documents/bike_share_projects/202108-divvy-tripdata.csv")
X202109_divvy_tripdata <- read_csv("Documents/bike_share_projects/202109-divvy-tripdata.csv")
X202110_divvy_tripdata <- read_csv("Documents/bike_share_projects/202110-divvy-tripdata.csv")
X202111_divvy_tripdata <- read_csv("Documents/bike_share_projects/202111-divvy-tripdata.csv")
X202112_divvy_tripdata <- read_csv("Documents/bike_share_projects/202112-divvy-tripdata.csv")
X202201_divvy_tripdata <- read_csv("Documents/bike_share_projects/202201-divvy-tripdata.csv")

join1 <- bind_rows(X202102_divvy_tripdata,X202103_divvy_tripdata,X202104_divvy_tripdata)
join2 <- bind_rows(X202105_divvy_tripdata,X202106_divvy_tripdata,X202107_divvy_tripdata)
join3 <- bind_rows(X202108_divvy_tripdata ,X202109_divvy_tripdata,X202110_divvy_tripdata)
join4 <- bind_rows(X202111_divvy_tripdata,X202112_divvy_tripdata,X202201_divvy_tripdata)

#combining all datasets
final_bike_share <- bind_rows(join1,join2,join3,join4)

summary(final_bike_share)

#CLEANING THE DATA

#Drop all NA
clean_final_bike_share <- drop_na(final_bike_share)

#creating new columns by separating dates into month, day, year and day of the week. 
clean_final_bike_share$date <- as.Date(clean_final_bike_share$started_at)
clean_final_bike_share$month <- format(as.Date(clean_final_bike_share$date), "%m")
clean_final_bike_share$day <- format(as.Date(clean_final_bike_share$date), "%d")
clean_final_bike_share$year <- format(as.Date(clean_final_bike_share$date), "%Y")
clean_final_bike_share$day_of_week <- format(as.Date(clean_final_bike_share$date), "%A")

clean_final_bike_share$ride_length <- difftime(clean_final_bike_share$ended_at,clean_final_bike_share$started_at)

# (descriptive statistics)
aggregate(clean_final_bike_share$ride_length~clean_final_bike_share$ member_casual,FUN = mean)
aggregate(clean_final_bike_share$ ride_length~clean_final_bike_share$ member_casual,FUN = median)
aggregate(clean_final_bike_share$ ride_length~clean_final_bike_share$ member_casual,FUN = max)
aggregate(clean_final_bike_share$ ride_length~clean_final_bike_share$member_casual,FUN = min)

#changing to days of the week 
clean_final_bike_share$day_of_week<-ordered(clean_final_bike_share$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

aggregate(clean_final_bike_share $ ride_length ~ clean_final_bike_share $ member_casual + clean_final_bike_share$day_of_week,FUN = mean)



# Number of rides between members and casual riders for each day of week
clean_final_bike_share %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

clean_final_bike_share$month <- ordered(clean_final_bike_share$month, 
                                        levels = c("January", "February", "March",
                                                   "April", "May", "June",
                                                   "July", "August", "September",
                                                   "Octobor", "November", "December"))


#tryout 
clean_final_bike_share$ride_length <- difftime(clean_final_bike_share$ended_at,clean_final_bike_share$started_at)

#Then the ride distance traveled in km
clean_final_bike_share$ride_distance <- distGeo(matrix(c(clean_final_bike_share$start_lng, clean_final_bike_share$start_lat), ncol = 2), matrix(c(clean_final_bike_share$end_lng, clean_final_bike_share$end_lat), ncol = 2))
clean_final_bike_share$ride_distance <- clean_final_bike_share$ride_distance/1000

#At last the speed in Km/h
clean_final_bike_share$ride_speed = c(clean_final_bike_share$ride_distance)/as.numeric(c(clean_final_bike_share$ride_length), units="hours")
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative:

clean_final_bike_share <- clean_final_bike_share[!(clean_final_bike_share$start_station_name == "HQ QR" | clean_final_bike_share$ride_length<0),]



#Fist we calculate the average distance, distance for both the casual and member type users:

userType_means <- clean_final_bike_share %>% group_by(member_casual) %>% summarise(mean_time = mean(ride_length),mean_distance = mean(ride_distance))

membervstime <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_time,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel time by User type",x="User Type",y="Mean time in sec")



#..............MEAN TRAVEL TIME BY USER TYPE  & MEAN TRAVEL DISTANCE BY USER TYPE.................................... 
membervsdistance <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Data by Motivate International Inc")

grid.arrange(membervstime, membervsdistance, ncol = 2)  



#..................Then we check  the number of rides diferences by weekday:......................................
clean_final_bike_share %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length),.groups = 'drop') %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type during the week",x="Days of the week",y="Number of rides",caption = "Data by Motivate International Inc", fill="User type") +
  theme(legend.position="top")




#..................................AVERAGE RIDING DURATION BY RIDER TYPE.................
my_theme = theme(plot.title=element_text(size=20),
                 axis.text.x=element_text(size=16), 
                 axis.text.y=element_text(size=16),
                 axis.title.x=element_text(size=18), 
                 axis.title.y=element_text(size=18),
                 strip.text.x=element_text(size=16), 
                 strip.text.y=element_text(size=16),
                 legend.title=element_text(size=18), 
                 legend.text=element_text(size=16))

options(repr.plot.width = 6, repr.plot.height = 8)

clean_final_bike_share %>% 
  group_by(member_casual) %>% 
  summarize(average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = average_duration)) +
  geom_col(position = "dodge") +
  labs(x = "Rider Type", y = "Average Duration (min)", 
       title = "Average Riding Duration by Rider Type") + my_theme

grid.arrange(clean_final_bike_share, ncol = 2) 



#............AVERAGE RIDING DURATION BY DAY: MEMBERS VS CASUAL RIDERS.....................
options(repr.plot.width = 10, repr.plot.height = 8)

clean_final_bike_share %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length/60), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders", caption = "Data by Motivate International Inc") + my_theme



#---------------------number of rides taken by each customer type-----------------

clean_final_bike_share %>%
  mutate(weekday = wday(started_at, label = TRUE))%>%
  group_by(member_casual, weekday)%>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length))%>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position="dodge") + 
  labs(title = "Number of rides by user type during the week", caption = "Data by Motivate International Inc")

#----------------------Mean travel distance by user type----------------------------

clean_final_bike_share <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Data by Motivate International Inc")

grid.arrange(clean_final_bike_share, ncol = 2)  




#.................AVERAGE NUMBERS OF RIDERS BY DAY: CASUAL RIDERS .......................

clean_final_bike_share %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders", caption = "Data by Motivate International Inc") + my_theme

#.................AVERAGE NUMBERS OF RIDERS BY MONTH: CASUAL RIDERS

options(repr.plot.width = 10, repr.plot.height = 8)
clean_final_bike_share %>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders") + my_theme


