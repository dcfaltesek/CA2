#this code produces the dataset and stock predictions for the NCAA Womens Tournament
#the data for this project was downloaded from College Sports Reference, they do amazing work 

#all A files are basic stats for a team
#all B files are basic opp stats for a team
#all C files are advanced stats for a team
#all D files are advanced opp stats for a team
#the B and D files can be used to infer defensive performance 

#you will need to change the file paths for your local use 
A21 <- read.csv("~/Desktop/A21.csv")
B21 <- read.csv("~/Desktop/B21.csv")
C21 <- read.csv("~/Desktop/C21.csv")
D21 <- read.csv("~/Desktop/D21.csv")
A22 <- read.csv("~/Desktop/A22.csv")
B22 <- read.csv("~/Desktop/B22.csv")
C22 <- read.csv("~/Desktop/C22.csv")
D22 <- read.csv("~/Desktop/D22.csv")
A23 <- read.csv("~/Desktop/A23.csv")
B23 <- read.csv("~/Desktop/B23.csv")
C23 <- read.csv("~/Desktop/C23.csv")
D23 <- read.csv("~/Desktop/D23.csv")
A24 <- read.csv("~/Desktop/A24.csv")
B24 <- read.csv("~/Desktop/B24.csv")
C24 <- read.csv("~/Desktop/C24.csv")
D24 <- read.csv("~/Desktop/D24.csv")

#combine the files to crete a year by year wide dataset
library(dplyr)
one<-bind_cols(A21,B21,C21,D21)
two<-bind_cols(A22,B22,C22,D22)
three<-bind_cols(A23,B23,C23,D23)
four<-bind_cols(A24,B24,C24,D24)

library(stringr)
#these functions clean the names strings
Z<-str_remove(one$School...2, "NCAA" ) %>% str_trim(side=c("both"))
onePrime<-data.frame("Team" = Z, "Year"=2021, one)
twoPrime<-data.frame("Team" = str_remove(two$School...2, "NCAA" ) %>% str_trim(side=c("both")), "Year"=2022, two)
threePrime<-data.frame("Team" = str_remove(three$School...2, "NCAA" ) %>% str_trim(side=c("both")), "Year"=2023, three)
fourPrime<-data.frame("Team" = str_remove(four$School...2, "NCAA" ) %>% str_trim(side=c("both")), "Year"=2024, four)


library(lubridate)
#break the tournament record dataset into individual years
#substantial debugging was needed as Basketball Reference uses different names on the bracket and in the dataset
#example: USC is Southern California 
A<-tournamentgamesWB %>% filter(Year == 2021)
B<-tournamentgamesWB %>% filter(Year == 2022)
C<-tournamentgamesWB%>% filter(Year == 2023)


#these joining routines made a very wide dataset where all four views for both teams in any given match are present
#check the data dictionary for more...
#join the left side
colnames(A)[2]<-"Team" 
V<-inner_join(A, onePrime, by="Team")
colnames(A)[2]<-"Left"
colnames(A)[3]<-"Team"
VV<-inner_join(A, onePrime, by="Team")                  
oneRead<-bind_cols(V, VV)
oneFinal<-bind_cols(A, oneRead)

#2022 year
colnames(B)[2]<-"Team" 
V<-inner_join(B, twoPrime, by="Team")
colnames(B)[2]<-"Left"
colnames(B)[3]<-"Team"
VV<-inner_join(B, twoPrime, by="Team")                  
twoRead<-bind_cols(V, VV)
twoFinal<-bind_cols(B, twoRead)


#2023 year
colnames(C)[2]<-"Team" 
V<-inner_join(C, threePrime, by="Team")
colnames(C)[2]<-"Left"
colnames(C)[3]<-"Team"
VV<-inner_join(C, threePrime, by="Team")                  
threeRead<-bind_cols(V, VV)
threeFinal<-bind_cols(C, threeRead)

#combine the 2021-2023 years into a single dataset
prepped_womens<-bind_rows(oneFinal, twoFinal, threeFinal)
#take a look to debug
View(prepped_womens)

#correct some names
colnames(prepped_womens)[2]<-"Left"
colnames(prepped_womens)[3]<-"Right"
colnames(prepped_womens)[4]<-"Result"
colnames(prepped_womens)[5]<-"Round"


#at this point I glimpse for the dat dictionary
glimpse(prepped_womens)

#a few renames to avoid chaos
colnames(prepped_womens)[8]<-"right.b"

#split for validation
library(rsample)
data_split <- initial_split(prepped_womens, prop = .8)

#for visibility
training_data <- training(data_split)
validation_data  <- testing(data_split)

#call the recipes 
library(recipes)
ball_rec <- 
  #this formula - how good is team left at 3s and turnovers, how good is right team at 3s and offensive rebounds
  recipe(Result ~ X3P....39 + TOV...48 + X3P....189 + ORB...193 , data = training_data) 

#quick check
ball_rec %>% prep()

#call the engine (you can use more complex methods if you choose)
library(parsnip)
engine<-rand_forest("classification") %>%
  set_engine("randomForest")

#call for workflows 
library(workflows)

#workflow base - engine and recipes 
ball_wflow<-
  workflow() %>% 
  add_model(engine) %>% 
  add_recipe(ball_rec)

#now fit the training data
ball_fit <- 
  ball_wflow %>% 
  fit(data = training_data)

#this function runs the prediction itself as one column and attaches the result
prediction=data.frame(.pred_class=predict(ball_fit, validation_data), truth=validation_data$Result)

#validation graphic
library(ggplot2)
prediction %>% 
  ggplot(aes(.pred_class, truth, colour = .pred_class!=truth))+geom_jitter()


#now to predict this years tournament you simply inner_join your bracket onto the current year data

#your new bracket is G
#your basic column names in G
#Year, Left, Right - 

#here is an example
#two vectors of teams
Left <- c("Iowa", "Stanford", "Oregon State")
Right <-c("Oregon", "South Carolina", "Minnesota")

#this dataframe has a year in it, and the left right conventions of our bracket conversions
#the higher seed line is LEFT
#notice that we have our entire input dataframe from the very start (if you dont, everything will be misaligned)
#you could include round in your prediction system if you really want
G<-data.frame("Year" = 2024, Left, Right, "Result" = "Nope", "Round" = "Quads")

#joiner routine
colnames(G)[2]<-"Team" 
V<-inner_join(G, fourPrime, by="Team")
colnames(G)[2]<-"Left"
colnames(G)[3]<-"Team"
VV<-inner_join(G, fourPrime, by="Team")                  
fourRead<-bind_cols(V, VV)
fourFinal<-bind_cols(G, fourRead)

#import the new data
#four final is the new processed data
prediction<-data.frame(.pred_class=predict(ball_fit, fourFinal), G)

prediction
#at least with my set seed, we have Iowa, South Carolina, and Oregon State winning


