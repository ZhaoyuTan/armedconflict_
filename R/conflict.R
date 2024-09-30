conflict <- read.csv(here("original","conflictdata.csv"), header = TRUE)

binary_conflict <- conflict %>%  group_by(ISO, year) %>% 
  summarise(death = sum(best)) %>% 
  mutate(armedconflict = ifelse(death < 25, 0, 1)) %>% 
  ungroup() %>% 
  mutate(year = year + 1) 


                     