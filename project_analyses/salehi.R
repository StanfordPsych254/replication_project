library(tidyr)
library(dplyr)

## MCF: note that Shima chose the followup ANOVA to be the primary test for power 
# analysis. but actually, on further 
# reflection we think it's the interaction that is most theoretically central. so we have revised
# to reflect that. (7/1/16)

path <- "data/salehi/"
files <- dir(path, 
             pattern = "*.json")
d.raw <- data.frame()

####Transforming data from JSON format to data.frame

#reading the variables of interest for each participants from .json files and turning them into one data.frame
for (f in files) {
  jf <- paste0(path,f)
  jd <- jsonlite::fromJSON(paste(readLines(jf), collapse=""))
  id <- data.frame(workerid = jd$WorkerId, 
                   trial_number_block=as.factor(jd$answers$data$trial_number_block),
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

#filtering data points for masculine data points
# masculine <- d %>% filter(traitType == 'Masculine')

#averaging ratings for each participant
# masculine <- masculine %>%
#   group_by(condition, workerid) %>%
#   summarize(workermean=mean(rating))

#one-way anova
# fitMasculine <- aov(workermean ~ condition, data = masculine)
# summary(fitMasculine)

#effect size
# masculineDivergentMean <- masculine %>% filter(condition == 'Divergent')
# masculineConnvergentMean <- masculine %>% filter(condition == 'Convergent')

### REVISED TO USE FULL ANOVA (MCF 7/1/16)
fitConTrait <- aov(rating ~ traitType * condition + 
                   Error(workerid/(traitType))+(condition), data=d)
summary(fitConTrait)

p <- summary(fitConTrait)[[2]][[1]][2,5]
Fval <- summary(fitConTrait)[[2]][[1]][2,4]
df1 <- summary(fitConTrait)[[2]][[1]][2,1]
df2 <- summary(fitConTrait)[[2]][[1]][3,1]

#effect size
# d %>%
#   group_by(workerid, condition, traitType) %>%
#   summarise(mean 
# masculineDivergentMean <- masculine %>% filter(condition == 'Divergent')
# masculineConnvergentMean <- masculine %>% filter(condition == 'Convergent')

test_stat <- paste0("F(",df1,",",df2,")=",round(Fval, digits=3))
source("project_analyses/computeES.R")
es <- esComp(Fval, df1 = df1, df2 = df2, esType = "F")

project_info <- data.frame(project_key = "salehi",
                           rep_final_n = length(unique(d$workerid)),
                           rep_n_excluded = 0, 
                           rep_es = es, 
                           rep_test_statistic_str = test_stat,
                           rep_t_stat = sqrt(Fval), #http://www.tc.umn.edu/~oakes007/Files/Comm%20Trials/F%20and%20t%20statistics.pdf
                           rep_t_df = df2,
                           rep_p_value = p)


# The result of mixed-model ANOVA for the original study was: A mixed-model
# ANOVA with condition as the between- subjects factor and trait type as the
# within-subjects factor revealed a main effect of trait type, F(1, 78) = 75.02,
# p < .001, ηp2 = .49, qualified by a marginally significant inter- action
# between trait type and condition, F(1, 78) = 3.05, p = .084, ηp2 = .04 

#Same as
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

# Original plot

se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

plot_mean = c(7.22,5.10,6.26,4.85) # From Paper
plot_se = c(1.26,2.04,1.43,1.56) / sqrt(80) # From Paper
traitType = c("Masculine-Agentic Traits","Feminine-Communal Traits","Masculine-Agentic Traits","Feminine-Communal Traits")
condition <- c("Divergent","Divergent","Convergent","Convergent")

o_plot <- data.frame(plot_mean,plot_se,traitType,condition)
colnames(o_plot) <- c('mean', 'se', 'traitType','condition')
o_plot$condition <- factor(o_plot$condition, levels=c("Divergent","Convergent"))
o_plot$traitType <- factor(o_plot$traitType, levels=c("Masculine-Agentic Traits","Feminine-Communal Traits"))

o_plot <- o_plot %>%
  mutate(upper = mean + se,
         lower = mean - se)

dodge <- position_dodge(width=0.9)
ggplot(o_plot, aes(x=condition, y = mean,fill = traitType)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(2.9,10.2)) + #to match original study
  xlab("Condition") +
  ylab("Centrality to Creativity") +
  ggtitle("Proudfoot - Original") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  scale_y_continuous(breaks=c(3:10)) +
  theme(legend.position=c(.5,.805),
        legend.direction= "vertical",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"))

ggsave("figures/salehi-original.png",width = 1.5,height=1.5,units="in")

# Replication plot
d_plot <- d %>% 
  group_by(traitType,condition) %>%
  summarise(mean = mean(as.numeric(rating),na.rm=T),
            se = se(as.numeric(rating)),
            upper = mean + se,
            lower = mean - se)

levels(d_plot$traitType) <- list("Masculine-Agentic Traits"="Masculine","Feminine-Communal Traits"="Feminine")

ggplot(d_plot, aes(x=condition, y = mean,fill = traitType)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(2.9,10.2)) + #to match original study
  xlab("Condition") +
  ylab("Centrality to Creativity") +
  ggtitle("Proudfoot - Replication") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  scale_y_continuous(breaks=c(3:10)) +
  theme(legend.position=c(.5,.805),
        legend.direction= "vertical",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"))

ggsave("figures/salehi-replication.png",width = 1.5,height=1.5,units="in")
