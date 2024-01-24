library(lubridate)
library(dplyr)
library(ggplot2)

#ok you know where we are going...
weddings %>% 
  ggplot(aes(Date, Experience))+geom_point()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

weddingsB<-weddings %>% 
  mutate(good_dates = mdy(Date))

weddingsB %>% 
  ggplot(aes(good_dates, Experience))+geom_point()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

#lets party
weddingsB %>% 
  ggplot(aes(good_dates, Experience, colour=Result==1))+geom_point()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

G<-weddingsB %>% 
  filter(Result ==1 & Experience <15)
View(G)

H<-weddingsB %>% 
  filter(Season ==3 & Episode ==1)
View(H)

#we can create some additional variables that we can use for analysis
weddingsC<-weddingsB %>% 
  #we dont need to reparse because good_dates is now officially a date
  mutate(yday = yday(good_dates)) %>% 
  mutate(year = year(good_dates))


weddingsC %>% 
  ggplot(aes(yday, Experience, colour=Result))+geom_point()

#we can also do this with continuous domain time
weddingsD<-weddingsC %>% 
  mutate(datetime = as.numeric(good_dates))

weddingsD %>% 
  ggplot(aes(datetime, Experience, colour=Result))+geom_point()

#lets get a little chaotic
weddingsD %>% 
  filter(datetime>15500 & datetime<15800) %>% 
  ggplot(aes(datetime, Season, colour=Episode))+geom_jitter()


