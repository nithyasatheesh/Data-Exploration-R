## IPL data analysis

## Load the data
data<-read.csv("D://Files//matches.csv")

## view first 6 observations
head(data)

## structure of the data
str(data)

## Summary statistics
summary(data)

## load required packages for Data manipulation
library(dplyr)

##1. What is the total number of matches played 
## and the total number of seasons?
# Total number of matches
data %>% summarise(match_count = n())

# Total number of seasons
season_count = length(unique(data$season))
season_count

##2.Which Team had won by maximum runs?
max_runs = data[which.max(data$win_by_runs),] 
max_runs %>% select('winner','win_by_runs')

data%>%filter(win_by_runs==max(win_by_runs))%>%select('winner','win_by_runs')

##3.Which Team(s) had won by maximum wicket?
data %>% filter(win_by_wickets == max(win_by_wickets)) %>%
  select('winner', 'win_by_wickets')

##4. Which season had most number of matches?
data %>% group_by(season) %>% summarise(match_cnt = n()) %>%
  filter(match_cnt == max(match_cnt))

library(ggplot2)
#data %>% group_by(season) %>% summarise(match_cnt = n()) %>% ggplot() + geom_bar(aes(season,match_cnt, fill = season), stat = 'identity')

x<-data %>% group_by(season) %>% summarise(match_cnt = n())
x
ggplot(x, aes(x=season, y=match_cnt,fill=season)) +
  geom_bar(stat="identity")+geom_text(aes(label=match_cnt),vjust=1.5,colour='white')+scale_x_continuous(breaks=seq(2008,2017,by=1))


##5. Which IPL Team is more successful?
data %>% group_by(winner) %>% summarise(winner_cnt = n()) %>%
  filter(winner_cnt == max(winner_cnt))

#visualization
success_team = data %>% group_by(winner) %>% summarise(winner_cnt = n())
ggplot(success_team,aes(winner,winner_cnt,fill=winner)) + geom_bar(stat = 'identity') +geom_text(aes(label=winner_cnt))+coord_flip()


##6.Find out count of matches played at each city

x<-data%>%group_by(city)%>%summarise(match_cnt=n())%>%arrange(desc(match_cnt))
#x<-data%>%group_by(city)%>%summarise(match_cnt=n())%>%arrange(desc(match_cnt))%>% slice(1:10)
ggplot(x,aes(city,match_cnt,fill=city))+geom_bar(stat = 'identity')+geom_text(aes(label=match_cnt)) + coord_flip()

##7.Find the match results of the season 2017
data%>%filter(data$season==2017)%>%group_by(result)%>%summarise(count=n())


# Checking the missing values in the data
colSums(is.na(data))
# Percentage format
colSums(is.na(data))/nrow(data)*100
sum(is.na(data$umpire3))/nrow(data)*100

# Less than 5% missing values--> drop the missing observations
# 5%-30%0r60% missing values--> do imputation
#More than 30% or 60% missing values--> drop the column.

#sum(is.na(data))/prod(dim(data))*100

## drop the column umpire 3
#data<-data[-18]
df<-subset(data, select = -umpire3)

#imputation
#data$umpire3[is.na(data$umpire3)]<-"unknown"
#colSums(is.na(data))


# Insights

#Mumbai Indians team had won by maximum runs.

#Mumbai Indians is the most successful team in IPL.

#2013 season had most number of matches
 
# 85 matches (most of the matches) played at Mumbai city.

#Chris Gayle has won the maximum number of player of the match title.

data %>% group_by(player_of_match) %>% summarise(winner_cnt = n()) %>%
  filter(winner_cnt == max(winner_cnt))



























boxplot(data$win_by_runs)
outlier_values <- boxplot.stats(data$win_by_runs)$out
