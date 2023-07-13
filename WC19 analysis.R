# inserting various packages needed for running all kinds of commands.


library(ggplot2)
library(scales)


# importing the excel data into R

library(readxl)
player_stats <- read_excel("C:/Users/DELL/Downloads/icc world cup 2019 batting stats.xlsx")

# Viewing the head that is first 6 rows of our dataset
head(player_stats)


attach(player_stats)

View(player_stats)
# finding the summary of the whole dataset

summary(player_stats)


# finding the structure of the whole dataset
str(player_stats)




# Determining the shape of data using Density plot

dens1 <- density(player_stats$Runs)

plot(dens1, col = "blue",xlab="Runs",main="DENSITY PLOT OF TOTAL RUNS") 

polygon(dens1, col = "red")




# A Boxplot depicting runs scored by each team in the world CUp 2019.

ggplot(player_stats, 
       aes(x = Team, y = Runs)) +
  geom_violin(fill = "blue") +
  geom_boxplot(width = .2, 
               fill = "yellow",
               outlier.color = "orange",
               outlier.size = 4) + 
  labs(title = "Runs scored by each team in the World Cup")+
  stat_summary(fun="mean",geom = "point",color="blue",size=2)


# Summarizing the mean runs scored by Left handed and Right Handed Batsmen

plotmean<-player_stats %>%
group_by(`Batting Hand`) %>%  #pipe operator
summarise(mean_runs = mean(Runs))
plotmean


# A bar graph showing the mean runs scored by both left handed and right handed batsmen.

ggplot(plotmean, 
       aes(x = factor(`Batting Hand`,
                      labels = c("Left Handed",
                                 "Right Handed")), 
           y = mean_runs)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  geom_text(aes(label = round(mean_runs)), 
            vjust = -0.25) +
  
  labs(title = "Mean Runs by Batting Hand", 
       subtitle = "Runs scored in 2019 World Cup",
       x = "Batting Hand",
       y = "Runs")



# A bar graph showing Total number of runs scored by each team in the World Cup.

ggplot(player_stats, 
       aes(x = factor(Team, labels = c("AFG","AUS","BAN","ENG","IND","NZ","PAK","RSA","SL","WI")),
           y = Runs), fill=Runs) +
  geom_bar(stat="identity",fill="steelblue") +
  labs(title = "Total Runs scored by each team",x="Teams",y="Runs")





# slicing the original dataset into a subset for easy analysis of data.
# Top 15 players are taken up from the original dataset.
top_15<-player_stats[c(1:15),]
top_15

data.frame(top_15)

# A bar graph depicting the Strike rate of Top 15 batsmen in the World CUp. 

ggplot(top_15, 
       aes(x = factor(Player, labels = c("Finch","Carey","Stokes","Babar","Warner","Morgan","Plessis","Root","Roy","Bairstow","Williamson","Rohit","Shakib","Smith","Kohli")), 
           y = SR)) +
  geom_bar(stat = "identity", 
           fill = "blue") +
  
  labs(title = "Strike Rate of Top 15 batsmen",
       x = "Players(Batsman)",
       y = "SR")


# A line chart depicting the boundary percentage of top 15 batsmen in the world cup 2019.

plot(top_15$BP,type="b",
     xlab="Rank of Batsman",ylab="Boundary Percentage",
     col = "red",main="Boundary Percentage of Top 15 Batsmen")


# Using if else condition finding out the tendencies of batsmen to attack and defend. 

player_stats$tendency=ifelse(player_stats$SR>90,
                             "Aggressive","Defensive")
player_stats$tendency


# A percentage bar chart showing the aggressive and defensive batsmen in each team which played the World Cup.

ggplot(player_stats, 
       aes(x = tendency,
           fill =Team)) + 
  geom_bar(position = "fill")+
  labs(title="Tendency of batsmen of different countries") 



# A bar graph showing Highest scores of batsman with attacking and defending approach.

ggplot(player_stats, 
       aes(x = factor(tendency,
                      labels = c("Aggressive",
                                 "Defensive")), 
           y = HS)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  scale_y_continuous(limits = c(0, 200))+
  labs(title = "Highest Score by Tendency to play", 
       x = "Tendency to play",
       y = "Highest Score")




#hyptothesis is an idea that can be tested

# null is that is to be tested
# alt is eveything else
#Pearson correlation test to find the correlation between two continuous(numeric) variables

#H0: The correlation coefficient = 0 or there is no linear relationship between the two variables
#Ha: The correlation coefficient is not equal to zero

cor.test(player_stats$Runs,player_stats$Ave,method = "pearson")

#p value<0.05
#We reject the H0
#Conclude that the correlation coefficient is not equal to zero. 
#Hence there exists a linear relationship between the 2 variables



#Chi square test for testing the association between 2 categorical (factor) variables

#H0:There is no association between two vars
#Ha:There is an association


chisq.test(player_stats$tendency, player_stats$tendency)

#p value <0.05
#Reject H0
#Conclude that there is an association between the 2 Aggressive approach and defensive approach


# One Sample t test

#Null Hypothesis and alternate hypothesis
#H0: mean = 5
#Ha: mean not equal to 5


t.test(player_stats$fours, mu=40)

# p value < 0.05
# Reject H0
# Conclude that mean value is not equal to 5
