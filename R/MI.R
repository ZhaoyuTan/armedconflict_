library(here)
library(tidyverse)
library(mice)
library(texreg)
finaldata <- read.csv(here("analytical", "finaldata.csv"), header = TRUE)

loggdp <- log(finaldata$gdp1000)

finaldata <- finaldata %>% mutate(loggdp)

midata <- finaldata |>
  mutate(ISOnum = as.numeric(as.factor(finaldata$ISO))) |>
  select(-country_name, -ISO)

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method

meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix

pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)
plot(mice.multi.out)


matmor.mi <- with(mice.multi.out,
                     matmorlm <- lm(matmor ~ -1 + armconf1 + loggdp + OECD + popdens + urban + 
                                      agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                                      as.factor(ISOnum) + as.factor(year)))
un5mor.mi <- with(mice.multi.out,
                  un5morlm <- lm(un5mor ~ -1 + armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                                   as.factor(ISOnum) + as.factor(year)))
infmor.mi <- with(mice.multi.out,
                  infmorlm <- lm(infmor ~ -1 + armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                                   as.factor(ISOnum) + as.factor(year)))
neomor.mi <- with(mice.multi.out,
                  neomorlm <- lm(neomor ~ -1 + armconf1 + loggdp + OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                                   as.factor(ISOnum) + as.factor(year)))

out.matmor <- pool(matmor.mi)
out.infmor <- pool(infmor.mi)
out.neomor<- pool(neomor.mi)
out.un5mor <- pool(un5mor.mi)

### CC analysis

preds <- as.formula(" ~ -1 + armconf1 + loggdp + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  as.factor(ISO) + as.factor(year)")

matmormod <- lm(update.formula(preds, matmor ~ .), data = finaldata)
un5mormod <- lm(update.formula(preds, un5mor ~ .), data = finaldata)
infmormod <- lm(update.formula(preds, infmor ~ .), data = finaldata)
neomormod <- lm(update.formula(preds, neomor ~ .), data = finaldata)

tosave <- list(out.matmor, out.infmor, out.neomor, out.un5mor, 
               matmormod, un5mormod, infmormod, neomormod)

keepvars <- list("armconf1" = "Armed conflict",
                 "loggdp" = "log(GDP)",
                 "OECD" = "OECD",
                 "popdens" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")
screenreg(list(matmormod, out.matmor, un5mormod, out.un5mor, infmormod, out.infmor, neomormod, out.neomor), 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Mat CC", "Mat MI", "Un5 CC", "Un5 MI", "Inf CC", "Inf MI", "Neo CC", "Neo MI"))

                                                                   