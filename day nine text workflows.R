#lets load some things
library(ggplot2)
library(dplyr)
library(recipes)
library(parsnip)
library(workflows)
library(rsample)
library(tune)
library(yardstick)
library(broom)
library(dials)
library(infer)
#go ahead and install this next one
library(textrecipes)


#our dataset today is a whole ton of music lyrics, we will want to predict who the artist is
#its really conveinent to do this with artist as the outcome var
muzak<-select(withusher, c(song_lyric, name))

#for our worktoday lets go hyper simplistic
data_split <- initial_split(muzak, prop = .8)

# Create data frames for the two sets:
training_data <- training(data_split)
validation_data  <- testing(data_split)

#pretty straightforward so far, we train on 660 songs, we validate on 166
#because this dataset is so SQEAKY clean, we can run our receipe very easily
#let's also be very clear here, we aren't going to cheat and predict artist ~ album
rec <- recipe(name ~ ., data = training_data) %>%
  #just words
  step_tokenize(song_lyric) %>%
  #LDA from 245
  step_lda(song_lyric, num_topics = 20) %>%
  prep() 

#juice that for proof of concept
juice(rec)

#lets visualize after a pivot...
A<-juice(rec)
B<-data.frame(Ref=1:dim(A)[1], A)

library(tidyr)
B %>% pivot_longer(cols=-c(Ref, name), names_to = "Topic")

#visually...
B %>% pivot_longer(cols=-c(Ref, name), names_to = "Topic") %>% 
  ggplot(aes(Ref, value, colour=Topic))+geom_point()+facet_grid(~name)


#NOW RUN IT AGAIN because your workflow doesnt like prepped objects
rec <- recipe(name ~ ., data = training_data) %>%
  #just words
  step_tokenize(song_lyric) %>%
#hit it with this reasonble LDA
  step_lda(song_lyric, num_topics = 20)

#lets assume our rec is good
engine<-rand_forest("classification") %>%
  set_engine("randomForest")


music_wflow<-
  workflow() %>% 
  add_model(engine) %>% 
  add_recipe(rec)
                
music_fit <- 
  music_wflow %>% 
  fit(data = training_data)



#result
#calling these .pred_class and truth is a Danism
prediction=data.frame(.pred_class=predict(music_fit, validation_data), truth=validation_data$name)

#confusion matrixx
prediction %>% 
  ggplot(aes(truth, .pred_class, colour= truth!=.pred_class))+
  #in this case you actually need to jitter to see the data
  geom_jitter()

#and numerically
result<-prediction %>% 
  group_by(.pred_class, truth) %>% 
  count() %>% 
  mutate("is this good?" = .pred_class==truth)

View(result)

#that is pretty funny, lets see which songs those were...
wut<-data.frame(.pred_class=predict(music_fit, validation_data), truth=validation_data$name, songs=validation_data$track_name)
View(wut)

#the text recepie is really flexible you can include lots of steps...
#tfidf could be a good next step...
