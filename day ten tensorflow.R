library(tensorflow)
library(keras)
library(ggplot2)
library(dplyr)
library(tidyr)

#this process really wants to make infereces from a dummy variable, so wide data, not the long you are used to
#first tho, lets add a reference variable
withusher<-withusher %>% 
  mutate(ref = 1:dim(withusher)[1])

lyrics_wide<-withusher %>% 
  pivot_wider(
    names_from = c(name),
    #the dataset does not include the id var, the album name, track, or lyric, thus we get six new columns
    #next to the columns where we have our critical song level metadata
    values_from = -c(ref, album_name, track_name, song_lyric))

#for our convenience we will work on pure vectors of outcome variables
six_wide_detect<-lyrics_wide%>%
  select(5:10)

six_wide_ready<-six_wide_detect %>% 
  mutate_all(~replace(., !is.na(.), 1)) %>% mutate_all(~replace(., is.na(.), 0))

#and these all need to be numeric, here is some base R nonsense to do that
six_wide_ready[, 1:6] <- sapply(six_wide_ready[, 1:6], as.numeric)

meta<-lyrics_wide %>% select(1:4)

#complete dataframe
lyrics_ready<-data.frame(meta, six_wide_ready, artist=withusher$name)
library(rsample)
#This is a 50/50 split method
data_split <- lyrics_ready%>%
  initial_split(prop = .5)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

## define vocab size (this is parameter to play with)
vocab_size = 50000

tokenizer <- text_tokenizer(num_words = vocab_size) %>% 
  fit_text_tokenizer(training_data$song_lyric)

training_seq <- texts_to_sequences(tokenizer, training_data$song_lyric)
valid_seq <- texts_to_sequences(tokenizer, validation_data$song_lyric)
all_seq<-texts_to_sequences(tokenizer, lyrics_ready$song_lyric)

#song 4 in words
training_data$song_lyric[399]
#lilwayne song 4 in numbers
training_seq[399]

library(stringr)
library(purrr)
## calculate training comments lengths
song_length <- training_seq %>% 
  map(~ str_split(.x, pattern = " ", simplify = TRUE)) %>% 
  map_int(length)

## plot comments length distribution
tibble(song_length = song_length)%>% 
  ggplot(aes(song_length))+
  geom_histogram(binwidth = 20)+
  theme_minimal()+
  ggtitle("Training data song length distribution")


## define max_len
max_len = 1000

x_train <- pad_sequences(training_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(valid_seq, maxlen = max_len, padding = "post")
x_all <- pad_sequences(all_seq, maxlen = max_len, padding = "post")

y_train <- training_data %>% 
  select(5:10) %>% 
  as.matrix()

## define embedding size
emd_size = 64

## define model
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = vocab_size, output_dim = emd_size) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 6, activation = "sigmoid")

summary(model)

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 100,
      batch_size = 16,
      validation_split = 0.05,
      verbose = 0) 


predicted_classes<-model %>% predict(x_test) %>% k_argmax()
#GO GET IT
classesX<-predicted_classes$numpy
predicted_classes
V<-as.numeric(predicted_classes)

#now our analysis
validator<-data.frame(V, validation_data)

#because this ran python on the back end, it is ZERO indexed
validator<-validator %>% 
  mutate(".pred_class" = if_else(V == 0, "Taylor Swift", ifelse(V==1, "Chili Peppers", 
                                                              ifelse(V==2, "Lil Wayne", ifelse(V==3, "Vulfpeck",
                                                                                              ifelse(V == 4, "Kanye West", "Usher"))))))

validator %>% 
  ggplot(aes(artist, .pred_class, colour= artist != .pred_class))+geom_jitter()



