library(dplyr)
library(ggplot2)
library(stringr)

glimpse(TV)

#networks as a factor
TV %>% 
  filter(Network == "NBC")

#rating and rank are continuous
TV %>% 
  ggplot(aes(Year, Rating))+geom_point()

#so what is year?

#but time is continuous... more on that later

#what is encoded in a string versus a factor like "NBC"
#three units, pretty clear

#what about longer things like show names
TV %>% 
  filter(str_detect(Program, "^A"))

#what about total letters
TV %>% 
  #just one with "ie"
  filter(str_detect(Program, "(i)e"))

#now lets put this all together...

TV %>% 
  #detector function
  
  #graphing function
