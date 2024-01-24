#how to make colors work well
#quick lets do some libraries
library(ggplot2)
library(dplyr)

#the examples are difficult because we need to align the GEOM with the TYPE

#lets use examples from the TV data
#getting up to speed
glimpse(TV)

#for our own convenience, let's clean up the TV dataset 

#the new TV dataset is called TV2
TV2<-TV %>% 
  #this is a specialied pipe which supposes that values within this VECTOR are what we are looking for
  # in this case our vector is a collection of strings
  filter(Network %in% c("ABC", "NBC", "CBS", "FOX"))

#let's do a simple geom point, as both year and rating are continuous
TV2 %>% ggplot(aes(Year, Rating, colour=Rank))+geom_point()

#lets store that for our own uses
A<-TV2 %>% ggplot(aes(Year, Rating, colour=Rank))+geom_point()

#because these are point we will use the color distiller to get a continuous, sequential color
A+scale_color_distiller(palette = "Greens")

#here is another fun one, lets mod up a distiller
jeep<-scale_colour_distiller(
  type = "seq",
  #another palate number
  palette = "Spectral",
  #go dark to light
  direction = 1,
  values = NULL,
  space = "Lab",
  #missing data are now pink
  na.value = "pink",
  guide = "colourbar",
  aesthetics = "colour"
)

#how goofy is that?
A+jeep

#we can also play with this via a gradient2
buss<-scale_colour_gradient2(
  low = "red",
  mid = "white",
  high = "navyblue",
  midpoint = 20,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour"
)

#how patriotic
A+buss

#a color scheme called truck, which has an extensive gradient
truck<-scale_colour_gradientn(
  colors=topo.colors(30),
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour",
)

A+truck

#classic
volkswagen<-scale_colour_gradientn(
  colors=rainbow(9),
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour",
)

A+volkswagen

#for continuous schemes
A+scale_color_continuous()
A+scale_color_gradient(low="green", high="yellow")
A+scale_color_gradient2(low="purple",mid="red",high="blue")

#continuous with blocks
A+scale_color_binned()
A+scale_color_steps(low="pink",high="red")
A+scale_color_steps2(low="yellow", mid="orange",high="lavender")
#brewer scales, continuous
A+scale_color_distiller(palette="Greens")
#brewer scales, binned continuous 
A+scale_color_fermenter(palette="Greens")

#the viridis series are great for folks with color differences
#b for binned
A+scale_color_viridis_b(option="H")

#c for continuous
A+scale_color_viridis_c(option = "plasma")

#this is the discrete one
A+scale_color_viridis_d()

#for your party needs
#"magma" (or "A")

#"inferno" (or "B")

#"plasma" (or "C")

#"viridis" (or "D")

#"cividis" (or "E")

#"rocket" (or "F")

#"mako" (or "G")

#"turbo" (or "H")



#now we can do this with some FILLS
#lets so one that is really tricky

#one discrete one continuous
B<-TV2 %>% ggplot(aes(Rank, Rating, colour=Rank, fill=Rank))+geom_col()
# we add our fill attribute for rainbow
B+volkswagen

#oh what about all of those weird blue interiors...

yugo<-scale_fill_distiller(
  type = "seq",
  palette = 7,
  direction = -1,
  values = NULL,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
)




#lets assign both
B+volkswagen+yugo
#that is just terrible, but very informative

#and now because we like to party, total nonsense
B+volkswagen+yugo+coord_polar()

#lets walk back a minute
#classic discrete only geom_bar
#a lovely fill with a 30 value rainbow
TV2 %>% ggplot(aes(Rank))+geom_bar(aes(fill=as.factor(Rank)))

C<-TV2 %>% ggplot(aes(Rating))+geom_bar(aes(fill=as.factor(Rank)))

#it will fail but you can see why 
C+scale_fill_brewer(palette="Blues")





#notice that this geom is coverly producing a summary statistic, geom_bar is counting stuff
#start by installing this new library from the CRAN
library(ggthemes)
??ggthemes

#whats in a theme
D<-TV2 %>% ggplot(aes(Year, Rating))+geom_hex()
D

#ok so its a continuous with a fill
D+scale_fill_viridis_c()

#how did it do that?
D+scale_fill_viridis_c()+theme_solarized_2()

#to get the base of our style
#this will reveal the underlying code
#notice I just did View() but I deleted the () that the dunction wanted to call
View(theme_dark)

#ok so a theme is really pretty straight forward, its just a ton of little variables to pass
What you can do is start with a style you like and then mod it
D+scale_fill_viridis_c()+theme(
  plot.background = element_rect(fill = "lightblue"),
  panel.grid = element_line(color = "lightgreen"))

#we can store that theme as well
ocean_spring<-theme(
  plot.background = element_rect(fill = "lightblue"),
  panel.grid = element_line(color = "lightgreen"))


plot <- TV2 %>% ggplot(aes(Year, Rating, colour = Rank)) +
  geom_point()


#declare a style
dan_color <- scale_fill_distiller(palette = "Purples")

dan_style<-theme(
    plot.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.2),
    panel.grid.major.x = element_line(color = "blue", linetype = "dashed"),
    panel.grid.minor.x = element_line(color = "lightblue", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "green", linetype = "dotted"),
    panel.grid.minor.y = element_line(color = "lightgreen", linetype = "dotted"),
    text = element_text(family = "YourFontName", color = "black")
  )
D+dan_color+dan_style

#lets try understanding a different way
#you all love this 
library(wesanderson)

#lets look at a few things
wes_palette("Royal1")
View(wes_palette("Royal1"))

beaver_colors <- c("black", "white", "orange", "yellow")
beaver_colors

#lets use this in context
TV2 %>% 
  ggplot(aes(Year, Rating, colour=as.factor(Network)))+geom_point()+scale_color_manual(values=beaver_colors)

#a continuous palatte
pal <- wes_palette(30, name = "Zissou1", type = "continuous")
pal
View(pal)

#for your wes anderson reference
names(wes_palettes)

TV2 %>% 
  ggplot(aes(Year, Network, colour=as.factor(Rank)))+geom_jitter()+
  scale_color_manual(values=pal)

#generally for continuous colors, go with a gradient
TV2 %>% 
  #notice a subtle change here
  ggplot(aes(Year, Rating, colour=Rank))+geom_jitter()+
  scale_colour_gradient2(low = "pink", high = "red")

#and another here...
TV2 %>% 
  #notice a subtle change here
  ggplot(aes(Year, Rating, colour=Rank))+geom_jitter()+
  scale_colour_gradientn(colours = terrain.colors(30))

set.seed(42)  # for reproducibility
random_colors <- sample(colors(), 30)
random_colors

# Example using scale_colour_gradientn with 30 random colors
TV2 %>% 
  ggplot(aes(Year, Rating, colour = Rank)) +
  geom_jitter() +
  scale_colour_gradientn(colors = random_colors, na.value = "grey50", values = scales::rescale(c(0, 1)))


#ok lets have some fun...
fightclub<-c("#AC7F84", "#1A1C16", "#BC435C", "#294742")


#playing with hues
E<-TV2 %>% 
  ggplot(aes(Year, Rank, color=as.factor(Network)))+geom_point()

E
#vs
E+scale_color_hue(l=30)

library(paletteer)
colorz<-paletteer_c("ggthemes::Orange", 30) 

#oranges
TV2 %>% 
  ggplot(aes(Year, Network, colour=as.factor(Rank)))+geom_jitter()+
  scale_color_manual(values=colorz)


#what if you need 50
colorzz<-paletteer_c("ggthemes::Orange", 50) 
colorzz

#names https://r-charts.com/color-palettes/