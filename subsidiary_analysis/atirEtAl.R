library(foreign)
library(readr)
library(dplyr)
library(lsr)

d <- read_csv("processed_data/atirEtAl.csv") %>%
  rename(accuracy = `accuracy cos`) %>%
  rename(overclaiming = `p(fa)`) %>%
  rename(trueKnowledge = `genuine knowledge`) %>%
  rename(questionOrder = `knowledge q 1st`) %>%
  mutate(questionOrder = factor(questionOrder, levels = c(1,2), 
                                labels = c("overclaimingFirst", "knowledgeFirst")))

str(d)
mean(d$overclaiming)
mean(d$spk)

mod <- lm(overclaiming ~ spk + accuracy, data = d)
etaSquared(mod)
