#you have seen this before furtively in 245
library(tidymodels)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

#import weddings...

#how many bins
# a couple of ways
weddings %>% 
  group_by(Experience) %>% 
  count()

weddings %>% 
  distinct(Experience)

as.factor(weddings$Experience)

#make a nice little plot
ggplot(weddings, aes(x = Experience)) + 
  geom_histogram(bins = 19, col= "white")


colorz<-c("purple", "blue", "lightblue", "goldenrod")
#stacked and styled bar...
ggplot(weddings, aes(x = Experience, fill=as.factor(Result))) + 
  geom_bar()+scale_fill_manual(values=colorz)

#now as a continuous
weddings %>% 
  ggplot(aes(Result, Experience, colour=ExpGiven))+geom_point()


interaction_fit <-  lm(Result ~ (ExpGiven + Budget.1)^2, data = weddings) 
interaction_fit

#the intercept was wonky for a reason
weddings %>% 
  ggplot(aes(Result, ExpGiven, colour=Budget.1))+geom_jitter()+geom_smooth()


#you can see that bride 1 had an amazing wedding and she hated bride 3
weddings %>% 
  filter(Episode == 5 & Season ==6)

#lets do a basic formula
analysis<-aov(Result ~ Budget.1 + ExpGiven, data=weddings)
plot(analysis)
summary(analysis)

#now with more factors
analysis<-aov(Result ~ Budget.1 + ExpGiven + ExpRank, data=weddings)
plot(analysis)
summary(analysis)

#now with interactions
analysis<-aov(Result ~ Budget.1 * ExpGiven * ExpRank, data=weddings)
plot(analysis)
summary(analysis)

