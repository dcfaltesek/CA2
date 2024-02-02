hot_long %>% 
  group_by(Song) %>% 
  count()


testing<-hot_long %>% 
  filter(Week.Position < 21| Previous.Week.Position == "NA") %>% 
  group_by(Song) %>% 
  count() %>% 
  filter(n >19)


View(testing)


hot_long %>% 
  filter(Song == "Big Girls Don't Cry")
