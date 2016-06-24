library(jsonlite)
library(tidyr)
library(dplyr)
# library(ggplot2)
# library(langcog) 
# library(rjson)
# library(tidyjson)
# library(car)
# library(effsize)
# library(compute.es)
# library(lme4)
# library(lsr)
# library(xlsx)
# library(knitr)
# library(png)
# library(grid)
# library(lmtest)
# library(lmerTest)
# library(gridExtra)

#functions for se and CI
sem <- function(x) {sd(x, na.rm=TRUE) / sqrt(length(x))}
ci95 <- function(x) {sem(x) * 1.96}

path <- "data/salehi/"
files <- dir(path, 
             pattern = "*.json")
d.raw <- data.frame()

####Transforming data from JSON format to data.frame

#reading the variables of interest for each participants from .json files and turning them into one data.frame
for (f in files) {
  jf <- paste0(path,f)
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  id <- data.frame(workerid = jd$WorkerId, 
                   trial_number_block=       as.factor(jd$answers$data$trial_number_block),
                   rating = as.numeric(jd$answers$data$rating),
                   gender = jd$answers$data$gender,
                   age = jd$answers$data$age,
                   sentence = jd$answers$data$sentence,
                   definition = jd$answers$data$definition,
                   political = jd$answers$data$political,
                   condition = as.factor(jd$answers$data$condition),
                   homelang = jd$answers$data$homelang,
                   income = jd$answers$data$income,
                   education = jd$answers$data$education,
                   ethnicity = jd$answers$data$ethnicity,
                   aim=jd$answers$data$expt_aim)
  d.raw <- bind_rows(d.raw, id)
}

# Number of participants
length(unique(d.b$workerid))

d <- d.raw %>%
  mutate(traitb = sentence %in% c('Decisive', 'Competitive', 'Self-reliant',   
                                  'Willing to take risks' , 'Ambitious', 'Daring' ,
                                  'Adventurous' , 'Courageous')) %>% 
  mutate(traitType = factor(ifelse(traitb == TRUE, "Masculine", "Feminine"),  
                            levels=c("Masculine","Feminine"))) %>%
  mutate(trait = factor(sentence))  %>%
  mutate(condition = factor(condition, 
                            levels=c("Divergent","Convergent"))) %>% 
  mutate(genderF = as.factor(gender))

write_csv(d, "processed_data/salehi.csv")

# ORIGINAL PAPER: The masculine traits were perceived as more central to
# creativity when divergence was emphasized, rather than convergence, F(1, 78) =
# 10.21, p = .002, d = 0.71.

# One Within Factors W1, One Between Factors B1  
fitConTrait <- aov(rating ~ (traitType*condition) + 
                     Error(workerid/(traitType)) + 
                     (condition),
                   data = d)
summary(fitConTrait)

project_info <- data.frame(project_key = "salehi",
                           rep_final_n = length(unique(d$workerid)),
                           rep_n_excluded = 0, 
                           rep_es = N, 
                           rep_test_statistic = N,
                           rep_p_value = N)


# The result of mixed-model ANOVA for the original study was: A mixed-model
# ANOVA with condition as the between- subjects factor and trait type as the
# within-subjects factor revealed a main effect of trait type, F(1, 78) = 75.02,
# p < .001, ηp2 = .49, qualified by a marginally significant inter- action
# between trait type and condition, F(1, 78) = 3.05, p = .084, ηp2 = .04 Same as
# the original study, this replication study suggests a significant main effect
# of trait type, F(1, 82) = 76.96, p < 0.0001, ηp2 = 0.47. However, unlike the
# original study in which the main effect of trait type was only marginally
# qualified with the condition, in the replication study the interaction between
# condition and trait type is significant, F(1,82) = 5.03, P = 0.03, ηp2 = 0.03.
# Also, both the main effect and interaction effect effect size are very close
# in the replication and original study. Therefore, the replication study echoed
# the main effect of the trait type with almost the same statistical
# significance level and effect size. These results suggest that same as the
# original study, in the replication study participants perceive masculine trait
# more central to creativity. However, in the replication study interaction of
# the trait type and condition has become more significant with slightly smaller
# effect size compared to the original study. Therefore, for the replication
# study, the gender bias was statistically more different between the two
# conditions, with higher gender bias in the divergent condition. This suggests
# that changes in the definition of creativity affect the gender biases of
# replication participants more than the initial participants of the original
# study. Besides the above mixed-model ANOVA, to confirm the findings, the
# authors in the original paper conducted two sets of follow-up tests: first,
# two repeated-measure ANOVA's within each condition to test the difference of
# rating of the two trait types within each condition; and second, two one-way
# ANOVA's for each trait type across conditions to test the effect of condition
# on the rating of each trait type. These follow-up tests have been repeated for
# the replication data in the following.
