
#CODE FOR PERFOMER 1
library(tidyr)
songz<-song_creators_wide_song_creators_wide_1_


library(dplyr)

performers<-songz %>% 
  select(c(Performer, Performer2, Performer3, Performer4, Performer5))


performers_long<-performers %>% 
  pivot_longer(cols=-Performer)

View(performers_long)

pl<-performers_long %>% 
  filter(value != "NA")

pef<-data.frame(SOURCE=pl$value, TARGET=pl$Performer)
write.csv(pef, "peformer_network.csv", row.names = FALSE)


#songwriters
writers<-songz %>% 
  select(c(songwriters,songwriter2,songwriter3,songwriter4,songwriter5,songwriter6,songwriter7,songwriter8,songwriter9,songwriter10,songwriter11,songwriter12,songwriter13,songwriter14,songwriter15,songwriter16,songwriter17,songwriter18,songwriter19,songwriter20,songwriter21,songwriter22,songwriter23,songwriter24,songwriter25,songwriter26,songwriter27,songwriter28,songwriter29,songwriter30))

A<-2:30
paste("songwriter",A, sep="", collapse=",")

library(tidyr)
w2<-writers %>% 
  pivot_longer(cols=-songwriters) %>% 
  filter(value != "NA")


writers<-data.frame(SOURCE = w2$value, TARGET = w2$songwriters)
write.csv(writers, "writers_network.csv", row.names = FALSE)

glimpse(songz)
without_label<-songz %>% 
  select(-c(`Annotation Specialist`, `Subjective Impression`, OneWord, label, label2, label3, label4, 14))


without_label<-without_label %>% 
  pivot_longer(-Song)
View(without_label)

without_label<-without_label %>% filter(value != "NA")
long_withoutlabel<-data.frame(SOURCE= without_label$Song, TARGET = without_label$value)

write.csv(long_withoutlabel, "long_withoutlabel.csv", row.names = FALSE)
