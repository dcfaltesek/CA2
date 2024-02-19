#step one for any model, after calling a mess of libraries is to check and split your data
library(dplyr)
library(readr)
TV <- read_csv("CA2B/TV.csv")
glimpse(TV)

#we know there is a foul data point, lets find it
TV %>% 
distinct(Network)

#now lets remove it
TV2<-TV %>% 
  filter(Network != "CBS/NBC")%>% 
  filter(Rating != "NA")

#now that our data are nice and clean we can split
library(rsample)
set.seed(97330)
data_split <- initial_split(TV2, prop = .8)

# Create data frames for the two sets:
training_data <- training(data_split)
validation_data  <- testing(data_split)

#step three: create a recepie
library(recipes)
TV_rec <- 
  #this is all the columns being used to predict Rating
  recipe(Rating ~ Type + Year + Network, data = training_data)

#step three.5: check that
TV_rec %>% prep()
TV_rec %>% check_missing()

#oh no, there is a missing value


#step four: set engine, this one does regression
library(parsnip)
rf_reg_spec <- 
  rand_forest(trees = 200, min_n = 5) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>% 
  set_engine("randomForest")

#step five combine the recepie and the engine specs
library(workflows)
TV_wflow <- 
  workflow() %>% 
  add_model(rf_reg_spec) %>% 
  add_recipe(TV_rec)

#step six fit the workflow
TV_fit <- 
  TV_wflow %>% 
  fit(data = training_data)

#step seven: predict the validation data

#this one is a little confusing as it is nested
#store this data frame as result
result<-data.frame(
  #this clause actually does the predicting
  prediction=predict(TV_fit, validation_data), 
  #this one runs and gets the true values from that dataset
  truth=validation_data$Rating)

#step eight: interpret the results
result %>% 
  group_by(.pred, truth) %>% 
  count()

#now for a visual analysis
#lets grab a diverging color scheme for ease
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

#visualize that
result %>% 
  ggplot(aes(.pred, truth, colour= truth-.pred))+geom_point()+jeep





