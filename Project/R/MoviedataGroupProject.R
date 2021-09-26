#' ---
#' title: "Movie Data Analysis"
#' author: "Amal Alappat, Lingeshwaran.C, Jayalakshmi,Thasneem Therodam Kandi"
#' date: "September 16, 2021"
#' ---

###' #### Import data
movieData<-read.csv("/Users/aliayyally/Documents/Data Analytics Training/Python/DataFiles/movies.csv")
head(movieData)
dim(movieData)
str(movieData)
colnames(movieData)
summary(movieData)

###' #### Import required libraries
library(scales)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(corrplot)

###' #### Checking the missing values
sum(is.na(movieData))
####' ##### There is 2370 null values

###' #### Lets check column wise null values
colSums(is.na(movieData))

###' #### Proportion of missing values
mean(is.na(movieData))

####' ##### Since the  the proportion of  null values is very small we can simply delete the null data 
movieData <- na.omit(movieData)
sum(is.na(movieData))

###' #### Now we have data without missing values, lest check dimension again
dim(movieData)

###' #### Visualizing duration and score using histogram
options(repr.plot.width=6,repr.plot.height = 4)
g1 <- ggplot(movieData,aes(x=runtime))+geom_histogram(binwidth = 5,fill="green4")
g2 <- ggplot(movieData,aes(x=score))+geom_histogram(binwidth = 5,fill="red")
grid.arrange(g1,g2,nrow=1,ncol=2)

###' #### Percentage of Movie Genre
ggplot(movieData,aes(x=genre,fill=genre))+geom_histogram(stat="count",binwidth = 1)+
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = 0),legend.position = "none")+
  labs(y="Percent",title="Percentage of Movie Genre")

####'##### Comedies, Action and Drama are highest genre of movies. Also, Adventure, Animation, Biography, Crime and Horror has considerable count of movies

###' ####  Now lets check the distribution of other Genre excluding above 8 genres
movieData %>% filter(!(genre %in% c("Action","Comedy","Drama","Adventure","Animation","Biography","Horror","Crime"))) %>%
  ggplot(aes(x=genre,fill=genre))+geom_histogram(stat="count",binwidth = 1)+
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = 0),legend.position = "none")+
  labs(y="Percent",title="Percentage of Movie Genre Other than top 8")

###' ####  Now, lets check the movies by countries 
####' ##### To understand how many movies produced by countries every year, lets create a data frame of year and countries with their  movie count

year_movies <- movieData %>%
  group_by(year,country) %>%
  summarize(movie_count=n())%>%
  filter(movie_count>=5)
head(year_movies)

###' ####  Plot the movie  number through time and color them with different country
options(repr.plot.width=4,repr.plot.height = 4)
ggplot(year_movies,aes(x=year,y=movie_count,colour=country))+
  geom_line(size= 2)+xlab("Year")+ylab("Number of  Movies")+theme_classic()
####' #####  United states movie production was drastically increased (upward) during 1980 and 1990 and also they are the largest players currently

###' ####  Top ten directors in terms of Score 
best_director<- movieData %>% group_by(director) %>%
  summarise(mean=mean(score))  %>%
  top_n(10) %>%
  arrange(desc(mean))
best_director
###' ####  Plotting the top 10 directors 
ggplot(best_director,aes(x=director, y= mean,alpha=mean))+
  geom_bar(stat = "identity",fill="darkorchid4")+labs(x="Best 10 Directors",y="Average Score")+
  ggtitle("Top 10 Directors with score")+coord_flip(ylim = c(8,8.5))


###' ####  Listing the top 10 actors based on the average movie score
best_actor<-movieData %>%
  group_by(star)%>%
  summarise(mean= mean(score))%>%
  top_n(10)%>%
  arrange(desc(mean))
best_actor

ggtitle("Top 10 actor with average score")+coord_flip(ylim=c(8.0,9.0))
ggplot(best_actor, aes(x = star, y = mean, alpha = mean))+
  theme(axis.text.x = element_text(angle = 90,hjust = .5,vjust = 0))+
  geom_bar(stat = "identity",fill="maroon4") + labs(x = "Top actor", y = "Average Score") 

###' ####  Top movie country with average score
best_moviecountry<-movieData %>%
  group_by(country)%>%
  summarise(mean= mean(score))%>%
  top_n(10)%>%
  arrange(desc(mean))
best_moviecountry

###' ####  Plotting the top movie country with average score
moviecountry<-ggplot(best_moviecountry, aes(x = country, y = mean, alpha = mean))+
  geom_bar(stat = "identity",fill = "turquoise4") + labs(x = "Top 10 movie country", y = "Average Score") + 
  ggtitle("Top movie country with average score")+coord_flip(ylim=c(7.0,9.0))
moviecountry

###' ####  Relationship between variables
numeric_col <- sapply(movieData, is.numeric)
movie_numeric<- movieData[,numeric_col]
###' ####  create correlation matrix
Correlation<-cor(movie_numeric)
corrplot(Correlation, method = "color")
Correlation
####' ##### Here from correlation, we noticed that, vote, gross and runtime are highly correlated with score

###' #### Lets create scatter plots to understand the relationship of the above variables better
ggplot(movieData, aes(x =votes, y =score))+
  geom_point(size=2) +
  stat_smooth(methos = lm, se = F, color = "red")+geom_smooth()+
  labs(title = "Votes Vs Score", 
       x = "votes", y = "score")+ ggtitle(paste("cor:", 0.945)) 

ggplot(movieData, aes(x =gross, y =score))+
  geom_point(size=2) +
  stat_smooth(methos = lm, se = F, color = "red")+geom_smooth()+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  labs(title = "Gross Vs Score", 
       x = "Gross", y = "Score")+ ggtitle(paste("cor:", 0.945)) 

ggplot(movieData, aes(x =runtime, y =score))+
  geom_point(size=2) +
  stat_smooth(methos = lm, se = F, color = "red")+geom_smooth()+
  labs(title = "Duration Vs. Score", 
       x = "Duration", y = "Score")+ ggtitle(paste("cor:", 0.945)) 

###' #### Lets categories the votes and then use it for colouring the above plots
movieDataC<-movieData
movieDataC$votes<-cut(movieDataC$votes, breaks = c(7,1000,10000,100000,1000000,2400000), labels = c("very few","few","middle","high","very high"))
summary(movieDataC$category)


ggplot(movieDataC, aes(x =runtime, y =score))+
  geom_point(size=2, aes(colour=votes)) +
  labs(title = "Duration Vs. Score and votes", 
       x = "Duration", y = "Score")

ggplot(movieDataC, aes(x =gross, y =score))+
  geom_point(size=2,aes(colour=votes))+
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  labs(title = "Gross Vs Score and votes", 
       x = "Gross", y = "Score")


####' ###### Conclusion:
####' Comedies, Action and Drama are mostly produced genre of movies
####' United states hold the highest stakes in movie production, US movie production was drastically increased (upward) during 1980 and 1990 and also they are the largest players currently
####' Roberto Benigni of Italy has the highest movie score average, 8.6 among the directors
####' Among the actors, "Mark Hamill" and "Philippe Noiret" are having the highest avg move score
####' Among the countries, Lebanon leads the chart of average move score. Surpisingly  US not in the list of top 10
####' vote, gross and runtime are the most determine factors of getting movie score


###' #### Linear Regression

###' #### Now, we identify the variables that are determinig the score, Let's select the determining variables
movies_important_variables = movieDataC[, c("score",
                                            "votes",
                                            "gross",
                                            "runtime")]

#regression plot - score vs gross

plot(score~gross, data=movies_important_variables,xaxt="none")

li = lm(score~gross, data=movies_important_variables)
print(li)
print(summary(li))
print(par(mfrow=c(2,2)))
plot(li)



i<-ggplot(movies_important_variables,aes(x=gross,y=score))+geom_point()
i<-i+geom_smooth(method='lm',col="red")
i<- i + scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))
print(i)



