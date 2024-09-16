rawdat <- read.csv("original/maternalmortality.csv",header = TRUE)

library(tidyverse)
rawdat <- rawdat %>% select(Country.Name,X2000:X2019) %>% rename_at(vars(matches("X")),~str_remove(.,"X"))
cleandat <- rawdat %>% pivot_longer(!Country.Name,names_to = "year",values_to = "MatMor")

cleandat$year <- as.numeric(as.character(cleandat$year))


install.packages("usethis")
library(usethis) 

usethis::use_git_config(user.name = "ZhaoyuTan", user.email = "tanzhaoyu1130@gmail.com")
usethis::use_git()
usethis::create_github_token()
usethis::use_github()


