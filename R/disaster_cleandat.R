install.packages("fastDummies")
library(tidyverse)
library(fastDummies)
disaster <- read.csv("original/disaster.csv", header = TRUE)
disaster %>% filter(Year>=2000 & Year<=2019, Disaster.Type%in%c("Earthquake","Drought")) %>% 
  select(Year,ISO,Disaster.Type) %>% rename(year=Year) %>% group_by(year,ISO) %>% 
  dummy_cols(select_columns = "Disaster.Type") %>% 
  rename(drought=Disaster.Type_Drought,earthquake=Disaster.Type_Earthquake) %>% 
  select(-Disaster.Type) %>% group_by(year,ISO) %>% summarise(drought = max(drought),
                                                              earthquake = max(earthquake)) %>% 
  ungroup() -> disaster_clean
