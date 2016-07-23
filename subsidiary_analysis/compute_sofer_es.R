rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(lsr)
sofer <- read_csv("../subsidiary_analysis/Experiment_1_Rps.csv")

d <- sofer %>%
  mutate(subid = 1:n()) %>%
  gather(datapoint, rating, starts_with("dft")) %>%
  separate(datapoint, into = c("foo","DFT","trial_num"), sep = "_") %>%
  mutate(condition = ifelse(Trust1Attrc2 == 1, "Trustworthiness", "Attractiveness"), 
         DFT = factor(DFT)) %>%
  select(-foo, -Trust1Attrc2) %>% 
  arrange(subid)

# Repeated measures
mod <- aov(rating ~ DFT * condition + Error(subid / DFT), data = d)
summary(mod)

# By subjects
subs <- d %>%
  group_by(condition, DFT, subid) %>%
  summarise(rating = mean(rating))

mod_subs <- aov(rating ~ DFT * condition, data = subs)
summary(mod_subs)

# By "items"
items <- d %>%
  group_by(condition, DFT) %>%
  summarise(rating = mean(rating))

mod_items <- aov(rating ~ DFT * condition, data = items)
summary(mod_items)


# linear model by items
items$nDFT <- scale(as.numeric(as.character(items$DFT)), scale=FALSE)
items$condition_e <- factor(items$condition)
contrasts(items$condition_e) <- c(-.5,.5)

mod_lm_items <- lm(rating ~ nDFT * condition_e + I(nDFT^2) * condition_e, data=items)
anova(mod_lm_items)
summary(mod_lm_items)

# linear models by subs 
items$nDFT <- scale(as.numeric(as.character(items$DFT)), scale=TRUE)
items$condition_e <- scale(ifelse(items$condition == "Trustworthiness", 1, 0), scale = F)
#contrasts(items$condition_e) <- c(-.5,.5)

mod_lm_items <- lm(scale(rating) ~ condition_e + nDFT + I(nDFT^2) + condition_e:nDFT + condition_e:I(nDFT^2) , data=items)
anova(mod_lm_items)
summary(mod_lm_items)
etaSquared(mod_lm_items)

# original t value on coefficient 
mixedModel <- lmer(rating ~ condition_e + nDFT + I(nDFT^2) + condition_e:nDFT + condition_e:I(nDFT^2) 
                   + (1 + nDFT + I(nDFT^2)| subid), 
                   data = d %>% 
                     mutate(nDFT = scale(as.numeric(as.character(DFT)), scale=TRUE)) %>%
                     mutate(condition_e = factor(condition)))
summary(mixedModel)
