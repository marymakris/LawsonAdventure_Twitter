library(Hmisc)
library(dplyr)

##Lawson Adenture Park Twitter Analysis

f <- read.csv("LawsonAdventure_tweets.csv")
head(f)
summary(f)

f$year <- factor(f$year)

#ANALYSES

sum(f$favorite_count)
sum(f$retweet_count)

mean(f$favorite_count)
mean(f$retweet_count)

#average favorites by month
avg_mo <- f %>% 
  group_by(month) %>% 
  summarize(AvgFavs=mean(favorite_count))

ggplot(avg_mo, aes(month, AvgFavs)) +       
  geom_bar(stat = 'identity') +
  labs(title = 'Average Number of Favorites by Month',
       x = 'Month',
       y = 'Average Number of Favorites') +
  theme_bw() + 
  scale_colour_brewer()

#count of posts by month
posts_month <- f %>% 
  group_by(month, year) %>% 
  summarise(count = length(month))

ggplot(posts_month, aes(month, count, fill = year)) + 
  geom_bar(stat='identity', position="dodge") +
  labs(title = 'Number of Tweets per Month',
       x = 'Month',
       y = 'Number of Tweets') +
  scale_x_continuous(breaks = seq(2, 12, by = 1)) +
  theme_bw()

# a way to concat yr/mth with a dash and add it to the data
posts_month <- within(posts_month, year_mth <- paste(year, month, sep='_'))
within(DF, C <- paste(A, B, sep='_'))

###Try the plot with a line.... still looks bad
ggplot(data=posts_month, aes(x = year_mth, y=count, group=1)) +
  geom_line(color="blue")+
  geom_point(color="blue")+
  labs(title = 'Number of Tweets per Month',
       x = 'Month',
       y = 'Number of Tweets')

###bar chart over time? no, also looks bad.. 

ggplot(posts_month, aes(year_mth, count)) +       
  geom_bar(stat = 'identity') +
  labs(title = 'Number of Tweets by Month & Year',
       x = 'Month',
       y = 'Number of Tweets') +
  theme_bw()

#average favorites by month
avg_mo <- f %>% 
group_by(month) %>% 
summarize(AvgFavs=mean(favorite_count))

ggplot(avg_mo, aes(month, AvgFavs)) +       
  geom_bar(stat = 'identity') +
  labs(title = 'Average Number of Favorites by Month',
       x = 'Month',
       y = 'Average Number of Favorites') +
  theme_bw() + 
  scale_colour_brewer()


### tried a lot of other analyses, but there wasn't enough data to provide anything else useful...





