maternal <- read.csv(here("original","maternalmortality.csv"),header = TRUE)

library(tidyverse)

library(usethis) 

maternal <- maternal %>% select(Country.Name,X2000:X2019) %>% rename_at(vars(matches("X")),~str_remove(.,"X"))

maternal_clean <- maternal %>% pivot_longer(!Country.Name,names_to = "year",values_to = "maternal mortality")

maternal_clean$year <- as.numeric(as.character(maternal_clean$year))


cleandata <- function(dataset,name){
  
  rawdata <- dataset %>% select(Country.Name,X2000:X2019) %>% rename_at(vars(matches("X")),~str_remove(.,"X"))
  cleandata <- rawdata %>% pivot_longer(!Country.Name,names_to = "year",values_to = name)
  cleandata$year <- as.numeric(as.character(cleandata$year))
  return(cleandata)
}

infant <- read.csv(here("original","infantmortality.csv"),header = TRUE)

infant_clean <- cleandata(infant,"infant mortality")

neonatal <- read.csv(here("original","neonatalmortality.csv"),header = TRUE)

neonatal_clean <- cleandata(neonatal,"neonatal mortality")

under5 <- read.csv(here("original","infantmortality.csv"),header = TRUE)

under5_clean <- cleandata(under5,"under5 mortality")



jointdata <- list(maternal_clean,infant_clean,neonatal_clean,under5_clean) %>% reduce(full_join)

install.packages("countrycode")
library(countrycode)
jointdata$ISO <- countrycode(jointdata$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")
