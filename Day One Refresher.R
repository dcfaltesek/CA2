#this is your quick review of how things work in R

#call your data from the lower right pane
library(readr)

#that happy homebrewed arrow is great, some folks are using a new version of that and the pipe
TV <- read_csv("TV.csv")

#we use libraries
library(ggplot2)

#it is often better practice to call specific functions without entire library
dplyr::filter(TV, Network == "ABC")

#TWO equals means IT IS
#generally you are safe to call full libraries in this class, there are known side effects in network science

#remember those happy things called pipes
library(dplyr)

netflix %>% 
  #we just want shows seen less than one HPV
  filter(HPV < 1) %>% 
  ggplot(aes(`Hours Viewed`, HPV))+geom_jitter()

#ok, this seems super weird
netflix %>% 
  #lets get the recrip of rank (so it starts at 1)
  mutate("rank" = 1/rank(`Hours Viewed`)) %>% 
  ggplot(aes(rank, `Hours Viewed`))+geom_point()
  
#TAKE THAT CHRIS ANDERSON
