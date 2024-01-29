hot_long %>% 
  group_by(Song) %>% 
  filter(Peak.Position<11) %>% 
  count() %>% 
  filter(n>10)