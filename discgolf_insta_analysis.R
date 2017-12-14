library(Hmisc)
library(dplyr)

#### Analysis of posts on instagram using #discgolf  ####

file <- read.csv("discgolf.csv")
file <- file[c(1:12)]
head(file)

head(sort.int(discgolf$Likes, decreasing=T), 10)

file$Time <- format(as.POSIXct(strptime(file$Date,"%b %d, %Y, %I:%M:%S %p",tz="")) ,format = "%H:%M")
file$Hour <- format(as.POSIXct(strptime(file$Time,"%H:%M",tz="")) ,format = "%H")
file$Min <- format(as.POSIXct(strptime(file$Time,"%H:%M",tz="")) ,format = "%M")
file$Date <- format(as.POSIXct(strptime(file$Date,"%b %d, %Y, %I:%M:%S %p",tz="")) ,format = "%m/%d/%Y")
file$Month <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%m")
file$Day <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%d")
file$Year <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%Y")

f <- as.data.frame(file)
head(f)

#ANALYSES

sum(f$Likes)

summary(f)

#overall average likes by month
avg_mo <- f %>% 
  group_by(Month) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_mo, aes(Month, AvgLikes)) +       
  geom_bar(stat = 'identity') +
  labs(title = 'Average Number of Likes by Month',
       x = 'Month',
       y = 'Average Number of Likes') +
  theme_bw() + 
  scale_colour_brewer()

#overall average likes by day of month
avg_day <- f %>% 
  group_by(Day) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_day, aes(Day, AvgLikes)) + 
  geom_bar(stat = "identity") +
  labs(title = 'Average Number of Likes by Day of Month',
       x = 'Day',
       y = 'Average Number of Likes') + 
  theme_bw()

#average likes by hour
avg_hr <- f %>% 
  group_by(Hour) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_hr, aes(Hour, AvgLikes)) + 
  geom_bar(stat = "identity") +
  labs(title = 'Average Number of Likes by Hour',
       x = 'Hour',
       y = 'Average Number of Likes') + 
  theme_bw()

#average likes by media type
avg_kind <- f %>% 
  group_by(Kind) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_kind, aes(Kind, AvgLikes, fill= Kind)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  labs(title = 'Average Instagram Likes by Media Type',
       x = 'Media Type',
       y = 'Average Number of Likes') +
  theme_bw()
