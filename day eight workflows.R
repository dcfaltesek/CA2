library(nycflights13)
library(dplyr)

set.seed(5)
flights<-sample_n(flights, 100000)


flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = lubridate::as_date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

#let's explore that
glimpse(flight_data)

#this is pretty nice for our needs as there is plenty of signal
flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

#this can be handy for debugging datasets
library(skimr)
flight_data %>% 
  skimr::skim(dest, carrier, origin) 

library(rsample)
data_split <- initial_split(flight_data, prop = .8)

# Create data frames for the two sets:
trainning_data <- training(data_split)
validation_data  <- testing(data_split)


library(recipes)
flights_rec <- 
  recipe(arr_delay ~ ., data = trainning_data) 

#base recepie
flights_prepped<-flights_rec %>% prep()

#you juice it after prep to see what the dataframe actually looks like
juice(flights_prepped)

#now for a more complex recepie

flights_rec <- 
  recipe(arr_delay ~ ., data = trainning_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_zv()

#prep it
flights_prepped<-flights_rec %>% prep()

#glimpse the juice to see what happened
glimpse(juice(flights_prepped))

#review
summary(flights_prepped)

#if you are satisfied with your receipe, its time for us to call an engine
library(parsnip)
#is a unified framework for calling dozens of engines  

#very straightforward model
lr_mod <- 
  logistic_reg() %>% 
  #this is default and requires no extension packages
  #that isn't to say that the others aren't a way to go
  #keras can be called from this function now...
  set_engine("glm")


#you can then define a workflow
library(workflows)
flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)


#you then FIT the workflow
flights_fit <- 
  flights_wflow %>% 
  fit(data = trainning_data)

#extract from parsnip lets you see all the sub-elements 
#for explanable modeling
library(tidyr)
flights_fit %>% 
  extract_fit_engine()

#then you predict it
result<-data.frame(prediction=predict(flights_fit, validation_data), truth=validation_data$arr_delay)

#confusion matrix...
library(ggplot2)
result %>% 
  ggplot(aes(.pred_class, truth, colour = .pred_class != truth))+geom_jitter()

#thats a bit messy, we might nee some numbers
result %>% 
  group_by(.pred_class, truth) %>% 
  count()

#diagnostic - was the binary for arrival delay appropriate?
#we need to take a big step back to read the continuous delay
flightsB<-data.frame(flights, "actual_delay" = flights$arr_delay)
#we can then pipe this in to see what we actually got

flight_dataB <- 
  flightsB %>%
  #just make one of those
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = lubridate::as_date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  #add our new one here, 
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour, actual_delay, month) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

#thats just a mess, no wonder it doesn't work
flight_dataB %>% 
  ggplot(aes(dep_time, actual_delay, colour=arr_delay))+geom_point()

#one more debuggggg
flight_dataB %>% 
  ggplot(aes(dep_time, actual_delay, colour=arr_delay))+geom_point()+facet_wrap(~month(ymd_hms(time_hour)))+guides(x = guide_axis(n.dodge = 3))


