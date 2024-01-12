#two geoms geoms geoms

#the book is really well done with good examples
#this one is from chapter 12

library(dplyr)
library(ggplot2)

ggplot(airquality, aes(Day, Temp, group = Month)) + 
  geom_line(aes(linewidth = Month)) + 
  scale_linewidth(range = c(0.5, 3))

#why do I like this plot?
#shows a collective geom and control of a fairly particular aesthetic, linewidth

#another good starter to get a sense of scale
ggplot(luv_colours, aes(u, v)) + 
  geom_point(aes(colour = col), size = 3) + 
  scale_color_identity() + 
  coord_equal()

#lets get a sense of the data here
glimpse(luv_colours)
#luv is very linear and consistent, its also the scheme for the base R named colors

#a little more fun
luv_colours %>% 
  mutate("space"=luv_colours$u<luv_colours$v) %>% 
  ggplot(aes(u, v)) +
  geom_point(aes(colour = col, size=L)) + 
  scale_color_identity() + 
  coord_equal()+
  scale_size(range = c(.1,5))+
  facet_grid(~space)

#two kinds of geoms 
#chapter 3 is an early draft but I like where the lesson is going 
#the structure of the demo dataset really drives the graphic

#here is my example: lets draw a pentagon
pentagon<-data.frame(
  a = c(3,3.5,3.75,3.5,3,2.75),
  b = c(1,1,2,3,3,2),
  c = c("a","b","c","d","e","f"))
  )

ggplot(pentagon, aes(a,b, label=c))+geom_path()

#this is my version of the height examples from chapter 4
TV %>% 
  filter(Rank <5) %>% 
  ggplot(aes(Year, Rating, group=Program, colour=Network))+
  geom_point(aes(size=1/Rank))+
  geom_line()

#by program and by overall trend
ggplot(TV, aes(Year, Rating)) + 
  geom_line(aes(group = Program)) + 
  geom_smooth(method = "lm", linewidth = 2, se = FALSE)  
  

#lets be more aesthetically sophisticated
ggplot(TV, aes(Year, Rating)) + 
  geom_line(aes(group = Program, colour=Network)) + 
  geom_smooth(method = "lm", linewidth = 2, se = FALSE)+
  facet_grid(~Network)+
  guides(x = guide_axis(n.dodge = 3))

#there are three kinds of geoms
#individual, collective, surfaces
#lets look carefully at the diamonds example







