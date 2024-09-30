
covariate <- read.csv(here("original", "covariates.csv"), header = TRUE)


source(here("R", "matmor_cleandat.R"))
source(here("R", "disaster_cleandatR"))
source(here("R", "conflict.R"))

final <- list(conflict, jointdata, disaster_clean) %>% 
  reduce(full_join, by = c('ISO', 'year'))

finaldata <- covariate %>% 
  left_join(final, by = c('ISO', 'year'))

finaldata <- finaldata %>% 
  mutate(armedconflict = replace_na(armedconflict, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         death = replace_na(death, 0))

write.csv(finaldata, file = here("analytical", "finaldata.csv"), row.names = FALSE)