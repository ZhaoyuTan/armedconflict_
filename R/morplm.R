library(here)
library(tidyverse)
library(plm)
library(stargazer)

finaldata <- read.csv(here("analytical", "finaldata.csv"), header = TRUE)

lmmod <- lm(matmor ~ -1 + armconf1 + gdp1000 + OECD + popdens + urban +
              agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year), 
            data = finaldata)
plmmod <- plm(matmor ~ armconf1 + gdp1000 + OECD + popdens + urban + 
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO",'year'),
              effect = "twoways",
              model = "within",
              data = finaldata)
list(lmmod,plmmod)

preds <- as.formula(" ~ armconf1 + loggdp + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmorlm <- lm(update.formula(preds, matmor ~ .), data = finaldata)
un5morlm <- lm(update.formula(preds, un5mor ~ .), data = finaldata)
infmorlm <- lm(update.formula(preds, infmor ~ .), data = finaldata)
neomorlm <- lm(update.formula(preds, neomor ~ .), data = finaldata)

loggdp <- log(finaldata$gdp1000)

finaldata <- finaldata %>% mutate(loggdp)

preds <- as.formula(" ~ armconf1 + loggdp + OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought + 
                  ISO + as.factor(year)")

matmorplm <- plm(update.formula(preds,matmor~.),
                 index = c("ISO",'year'),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
un5morplm <- plm(update.formula(preds, un5mor ~ .), 
                 index = c("ISO",'year'),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
infmorplm <- plm(update.formula(preds, infmor ~ .), 
                 index = c("ISO",'year'),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)
neomorplm <- plm(update.formula(preds, neomor ~ .), 
                 index = c("ISO",'year'),
                 effect = "twoways",
                 model = "within",
                 data = finaldata)

stargazer(matmorplm,un5morplm,infmorplm,neomorplm,type = 'text',ci=TRUE,ci.level=0.95)
