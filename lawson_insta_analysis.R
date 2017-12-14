library(ggplot2)
library(dplyr)

#### Analysis of Instagram posts by Lawson Adventure Company  ####

file <- read.csv("lawsonadventure.csv")
head(file)

colnames(file)

head(sort.int(discgolf$Likes, decreasing=T), 3)

file$Time <- format(as.POSIXct(strptime(file$Date,"%b %d, %Y, %I:%M:%S %p",tz="")) ,format = "%H:%M")
file$Hour <- format(as.POSIXct(strptime(file$Time,"%H:%M",tz="")) ,format = "%H")
file$Min <- format(as.POSIXct(strptime(file$Time,"%H:%M",tz="")) ,format = "%M")
file$Date <- format(as.POSIXct(strptime(file$Date,"%b %d, %Y, %I:%M:%S %p",tz="")) ,format = "%m/%d/%Y")
file$Month <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%m")
file$Day <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%d")
file$Year <- format(as.POSIXct(strptime(file$Date,"%m/%d/%Y",tz="")) ,format = "%Y")

l <- as.data.frame(file)
head(l)

#ANALYSES

summary(l)
sum(l$Likes)
sum(l$Comments)

#count of posts per month 
num_posts <- l %>% 
  group_by(Month, Year) %>% 
  summarise(count=length(Posts))

ggplot(num_posts, aes(Month, count)) +       
  geom_bar(stat = 'identity') +
  labs(title = 'Average Number of Likes by Month',
       x = 'Month',
       y = 'Average Number of Likes') +
  theme_bw() + 
  scale_colour_brewer()

#overall average likes by month
avg_mo <- l %>% 
  group_by(Month) %>% 
  summarize(AvgLikes=mean(Likes))

#overall average likes by day of month
avg_day <- l %>% 
  group_by(Day) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_day, aes(Day, AvgLikes)) + 
  geom_bar(stat = "identity") +
  labs(title = 'Average Number of Likes by Day of Month',
       x = 'Day',
       y = 'Average Number of Likes') + 
  theme_bw()

#average likes by hour
avg_hr <- l %>% 
  group_by(Hour) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_hr, aes(Hour, AvgLikes)) + 
  geom_bar(stat = "identity") +
  labs(title = 'Average Number of Likes by Hour',
       x = 'Hour',
       y = 'Average Number of Likes') + 
  theme_bw()

#average likes by media type
avg_kind <- l %>% 
  group_by(Kind) %>% 
  summarize(AvgLikes=mean(Likes))

ggplot(avg_kind, aes(Kind, AvgLikes, fill= Kind)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  labs(title = 'Average Instagram Likes by Media Type',
       x = 'Media Type',
       y = 'Average Number of Likes') +
  theme_bw()

#average comments by media type... didn't use, basically the same for slides and photos
avg_kind2 <- l %>% 
  group_by(Kind) %>% 
  summarise(AvgCom=mean(Comments))

ggplot(avg_kind2, aes(Kind, AvgCom, fill= Kind)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  labs(title = 'Average Instagram Comments by Media Type',
       x = 'Media Type',
       y = 'Average Number of Comments') +
  theme_bw()
