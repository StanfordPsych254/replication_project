rm(list=ls())
# library(tidyr)
# library(dplyr)
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
                           rep_es = , 
                           rep_test_statistic = 
                           rep_p_value = )


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

### Confirmatory analysis 2: repeated measure anova for each condition

The authors have conducted a repeated measure ANOVA to compare the rating for masculine versus feminine traits within the divergent condition. this analysis for the replication data have been reported in the following:
  
  #### Confirmatory analysis 2.1: different trait types' rating within the divergent condition
  
  ```{r Confirmatory analysis 2.1: trait type rating within the divergent condition }
#filtering the data points for the divergent condition
divergent <- d.b2 %>% filter(condition == 'Divergent') 

#effect of trait types in divergent condition: Within-subject anova
# One Within Factor: fit <- aov(y~A+Error(Subject/A),data=mydataframe)
fitDivergent <- aov(rating ~ traitType + Error(workerid/traitType), data=divergent)
summary(fitDivergent)

#effect size
#filtering the data points for the masculine traits
divergentMasculine <- divergent %>% filter(traitType == 'Masculine')

#filtering the data points for the feminine traits
divergentFeminine <- divergent %>% filter(traitType == 'Feminine')

#effect size for the the difference in the rating of masculine and feminine traits within the divergent condition
cohen.d(divergentMasculine$rating,divergentFeminine$rating)
```


Same as the divergent condition, for the convergent condition, the authors have conducted a repeated-measure ANOVA to compare the rating for masculine versus feminine traits in this condition. This analysis for the replicated data has been reported in the following:
  
  #### Confirmatory analysis 2.2: different trait types' rating within the convergent condition
  
  
  ```{r Confirmatory analysis 2.2: trait type rating within the convergent condition}
#filtering the data points for the convergent condition
convergent <- d.b2 %>% filter(condition == 'Convergent')

#effect of trait types in Divergent condition: Within-subject anova
# One Within Factor: fit <- aov(y~A+Error(Subject/A),data=mydataframe)
fitConvergent <- aov(rating ~ traitType + Error(workerid/traitType), data=convergent)
summary(fitConvergent)

#effect size
#filtering the data points for the masculine traits
convergentMasculine <- convergent %>% filter(traitType == 'Masculine')

#filtering the data points for the feminine traits
convergentFeminine <- convergent %>% filter(traitType == 'Feminine')

#effect size for the the difference in the rating of masculine and feminine traits within the convergent condition
cohen.d(convergentMasculine$rating,convergentFeminine$rating)
```

The results of these two tests in the original study was:
  
  > 
  [P]articipants associated creativity more with stereo- typically masculine-agentic personality traits than with stereotypically feminine communal personality traits in both the divergent-thinking condition (M = 7.22, SD = 1.26, vs. M = 5.10, SD = 2.04), F(1, 78) = 54.18, p < .001, d = 1.25, and the convergent-thinking condition (M = 6.26, SD = 1.43, vs. M = 4.85, SD = 1.56), F(1, 78) = 23.90, p < .001, d = .94.

The replication tests similarly revealed that masculine traits have been associated with creativity more strongly than feminine traits in both the divergent condition (M = 7.12, SD = 1.98, vs. M = 4.92, SD = 2.32), F(1, 42) = 74.81, p < 0.0001, d = 1.02, and the convergent condition ( M = 6.66, SD = 1.91, vs. M = 5.35 , SD = 2.17), F(1, 40) = 17.25, p = 0.0002, d = 0.64.

Therefore, Although same as the original study, in the replication study the difference of rating for masculine versus. feminine was significant in both conditions, the effect sizes were smaller in the replication data.

Note: I believe the degree of freedom of ANOVA's have not been reported correctly in the original study.

### Confirmatory analysis 3: one-way anova for each trait type 

Besides looking at the difference between rating of the two trait types' within each condition, the authors have examined the difference of each trait type rating across conditions. Therefore, they have conducted two one-way ANOVA's for each trait type i.e. feminine and masculine to check the effect of condition on the ratings of each trait type. This is a test of whether the changes in the definition of creativity affect how central participants perceive each trait type to the creativity.

To test whether the changes in the definition of creativity affect how central  participants perceived masculine trait to creativity, a one-way ANOVA on the ratings of masculine traits over condition has been conducted. This analysis for the replicated data has been reported in the following.


#### Confirmatory analysis 3.1: the masculine traits' rating across conditions (the main test for power analysis )


```{r Confirmatory analysis 3.1: The masculine trait type rating across conditions}
#filtering data points for masculine data points
masculine <- d.b2 %>% filter(traitType == 'Masculine')

#averaging ratings for each participant
masculine <- masculine %>%
  group_by(condition, workerid) %>%
  summarize(workermean=mean(rating))

#one-way anova
fitMasculine <- aov(workermean ~ condition, data = masculine)
summary(fitMasculine)

#effect size
#filtering the divergent data points
masculineDivergentMean <- masculine %>% filter(condition == 'Divergent')

#Filtering the conergent data points
masculineConnvergentMean <- masculine %>% filter(condition == 'Convergent')

#effect size of condition on ratings of masculine traits
cohen.d(masculineDivergentMean$workermean, masculineConnvergentMean$workermean)
```

The results of this test for the original study was:
  
  > 
  Furthermore, the masculine traits were perceived as more central to creativity when divergence was emphasized, rather than convergence, F(1, 78) = 10.21, p = .002, d = 0.71, which indicates that the association between creative thinking and masculinity is most pronounced when creativity is conceptualized as seeing the world differently than other people do and as generating ideas that diverge from norms and traditions.

Unlike the original study with the significant difference in the ratings of masculine traits across conditions, in the replicated study, this difference was marginally significant with a smaller effect size, F(1, 82) = 2.96, p = 0.09, d = 0.38. Therefore, it seems that in the replication study, emphasizing creativity as more about diverging from others than converging to others marginally affects how central participants perceived masculine traits to creativity. However, in the original study this change in the definition had a significant effect on the rating of masculine traits.

#### Confirmatory analysis 3.2: the feminine traits' rating across conditions

In the original study, also in order to test whether the changes in the creativity definition influence how central participants perceive feminine traits to creativity, a one-way ANOVA of feminine trait ratings over condition had been conducted. This analysis for the replication data have been reported here:
  
  ```{r Confirmatory analysis 3.2: the feminine trait rating across conditions}
#filtering feminine traits data points
feminine <- d.b2 %>% filter(traitType == 'Feminine')

#averaging ratings across participants
feminine <- feminine %>%
  group_by(condition, workerid) %>%
  summarize(workermean=mean(rating))

#one-way anova
fitFeminine <- aov(workermean ~ condition, data = feminine)
summary(fitFeminine)

#one-way anova
feminineDivergentMean <- feminine %>% filter(condition == 'Divergent')
feminineConnvergentMean <- feminine %>% filter(condition == 'Convergent')

#effect size
cohen.d(feminineDivergentMean$workermean, feminineConnvergentMean$workermean)
```

The results of the original study was that:
  
  > 
  There was no effect of condition on the perceived centrality of the feminine traits, F < 1, p = .536.

Same as the original study, in the replication study, changes in the conceptualization of creativity did not affect how central participants perceived feminine traits to creativity, F(1, 82) = 1.2, p = 0.28. However, although insignificant, the patter of this change should be noticed that it is in the opposite direction of rating for masculine traits, Cohen's d = -0.24. 


Overall, according to the data of the replication and the original study, the average ratings for feminine traits for both conditions were very close to the mid point of the provided scale. This implies that participants had no strong positive or negative association for feminine traits and creativity, and also changes in conceptualization of creativity would not drastically change this pattern. However, for masculine traits there is a positive association between these traits and creativity, and furthermore, conceptualizing creativity more aligned with masculine traits could marginally amplify this positive association. 

##II. Exploratory analyses: mixed-effect regression model

The following graph shows the average ratings for each trait across conditions to depict the variations of ratings even for traits within the same trait type.

```{r ggplot of average rating for each trait}
#barplot for the average rating of each feminine traits across conditions
feminine.traits <- d.b2 %>% filter(traitType=='Feminine') %>%
         group_by(trait,traitType, condition, workerid) %>%
         summarize(x=mean(rating)) %>%
         summarize(Feminine.Rating=mean(x), sem= sem(x)) %>%
         ggplot(., aes(x = condition,y = Feminine.Rating, fill=condition)) +
         geom_bar(position = 'dodge',stat ='identity', width = 0.5) + 
         geom_errorbar(aes(ymin = Feminine.Rating-sem, 
                           ymax = Feminine.Rating+sem),
                       width = .2,                   
                       position = position_dodge(0.5)) + 
         scale_fill_manual(values=c("#FF6633", "#33CCCC")) +
         facet_grid(.~trait, scales = "free", space = "free") +
         ylim(0,9) +
         theme(strip.background = element_blank(),
               axis.text.x=element_blank(), 
               axis.ticks=element_blank(),  
               legend.title=element_blank(), 
               axis.title.x=element_blank()) 

#barplot for the average rating of each masculine traits across conditions
masculine.traits <- d.b2 %>% filter(traitType=='Masculine') %>%
         group_by(trait,traitType, condition, workerid) %>%
         summarize(x=mean(rating)) %>%
         summarize(Masculine.Rating=mean(x), sem= sem(x)) %>%
         ggplot(., aes(x = condition,y = Masculine.Rating, fill=condition)) +
         geom_bar(position = 'dodge',stat ='identity', width = 0.5) + 
         geom_errorbar(aes(ymin = Masculine.Rating-sem, 
                           ymax = Masculine.Rating+sem),
                           width = .2,                   
                           position = position_dodge(0.5)) + 
        scale_fill_manual(values=c("#FF6633", "#33CCCC")) +
        facet_grid(.~trait, scales = "free", space = "free") +
        ylim(0,9) +
        theme(strip.background = element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks=element_blank(), 
        legend.title=element_blank(), 
        axis.title.x=element_blank()) 
 

#ploting the masculine and feminine traits' rating in a grid
grid.arrange(feminine.traits, masculine.traits, ncol=1)
```

Based on this graph, there is a variation in the mean rating of the two conditions across traits, even for the traits within a same trait type. This justifies the possibility of having a mixed-effect model with a random intercept for each trait. Furthermore, although the direction of change for rating across conditions is the same for each trait within a same trait type, the scope of this change varies slightly across traits. This justifies the possibility of having a random slope for condition for each trait in a mixed-effect model. However, it should be mentioned that the evidence for random intercepts is stronger than random slopes, at least visually. 

To test these speculations about random slope and intercept for each trait and how controlling for these random effects might affect the results, a mixed-effect regression model approach can be used. Furthermore, the mixed-effect model allows for controlling the random variations across participants as well.

### Exploratory analyses 1: repeating mixed-model anova analysis using mixed-effect regression model

In the following the mixed-model ANOVA analysis has been repeated using a mixed-effect regression model. To do so, through the stepwise procedure randome  intercept and slope for both traits and subjects have been added to the model, to determine which mixed-effect model better fits the data. for all of the models, rating has been normalized to make the interpretation of coefficients easier.

```{r random effect of traits}
#random intercept for each traits
model1.1 <- lmer(scale(rating) ~ traitType * condition + (1 | trait), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model1.1)$coef)

#random intercept & slope for each traits
model1.2 <- lmer(scale(rating) ~ traitType * condition + (1 + condition| trait), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model1.2)$coef)

#comparing the fit of the models
lrtest(model1.1 , model1.2)
```

According to the likelihood-ratio test,compared to a model with only random intercept, having a model with both random intercept and slope for traits would not improve the fit of the model significantly , X2(2) < 0.001, p =0.99. Therefore, for  traits, only a random intercept will be included in the model. This decision can be confirmed by the graph of the average ratings for each trait; in this graph most of the variation for each trait was due to the mean rating of the two conditions rather than how the ratings were different across the conditions.

```{r random effect of traits and subjects}
#random intercept & slope for each trait + random intercept for each subject
model1.3 <- lmer( scale(rating) ~ traitType * condition + (1 | trait) +  (1 | workerid), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model1.3)$coef)

#comparing the fit of the models
lrtest(model1.1 , model1.3)

#random intercept for each trait + random intercept & slope for each subject
model1.4 <- lmer(scale(rating) ~ traitType * condition + (1 | trait) +  (1 + traitType | workerid), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model1.4)$coef)

#comparing the fit of the models
lrtest(model1.3 , model1.4)
```

Furthermore, including the random intercept for each subject will significantly improves the fit of the model, X2(1) = 356.2, p < 0.0001. Also, adding the random slope of trait type for each subject will enhance the fit of the model further, X2(2) = 253.74, p < 0.0001. Therefore, the model that best captures the variance of the data is the one with the random intercept for each trait, and a random intercept and slope of a trait type for each subject. 

This model suggests that first even within each trait type, each trait is perceived to have different association with creativity. Second, within each condition, there is a variation for each subject of how central they perceived each trait type to creativity and third each subject is affected by the trait type differently. To better determine the effect of trait type and condition on the creativity gender bias, we should control for these three sources of variations.


According to the mixed-effect model with random intercept for each trait and random intercept and slope for each subject, there is a significant main effect of trait type, b(feminine) = -0.96, t(45.4) = -5.56, p < 0.0001. This result echoes the result of the original study and the above mentioned confirmatory analysis that participants perceived masculine traits more central to creativity than feminine traits. In the divergent condition, the difference in the ratings of the masculine and feminine traits are about one standard deviation of ratings, which is considerable gap. Furthermore, in the mixed-effect model, the main effect of trait type is significantly modified by condition, b(feminine*convergent) = 0.39, t(82) = 2.24, p = 0.03. This interaction implies that by using the convergent definition of creativity, participants perceived the feminine trait type significantly more central to creativity, 0.39 standard deviation increase. Also, unlike the original study and the above ANOVA's analyses, in this model the main effect of the condition is marginally significant, b = -0.2, t(82) = -1.72, p = 0.09. This implies that when the convergent definition of creativity is used, the masculine trait is rated 0.2 standard deviation less central to creativity.

In sum, these results echoe the finding of the original study that there exists an implicit gender bias in attribution of creativity favoring the stereotypical masculine traits. That said, the mixed-model approach shows that if the random effects of each participant and each trait are controlled, then this gender bias found to be more influenced by conceptualization of creativity than what was observed in the original study as well as the above mixed-model ANOVA. By using a definition of creativity more aligned with feminine and communal traits, participants perceive the feminine traits more central to creativity. The size of this increase is also larger than the decrease in masculine traits' rating using convergent condition. That said, this effect of conceptualization is not strong enough to over ride the implicit gender bias fully- it only offsets about half of  it (0.39/(0.96-0.2)).


Besides replicating the test for the effects of trait type and condition, it would be informing to include eduction, ethnicity, and gender of participants in the analyses to control for the effects of these characteristics of participants on how they evaluate the centrality of a trait to creativity. It might be the case that the definition of creativity is gendered to different extent for female and male participants. Also, level of education might influence how participants perceive creativity. Furthermore, different ethnicity with different cultures might have different definitions for creativity. It might not be surprising that participants with collectivist cultures consider creativity more aligned with communal i.e. feminine traits.

### Exploratory analyses 2: gender effect 

To test the variation of creativity implicit gender bias across participants, to the earlier model, the gender of participants has been added.

```{r Exploratory analyses 2: gender effect}
#model with trait type and condition + gender
model2.1 <- lmer(scale(rating) ~ traitType * condition + gender + (1 | trait) +(1 +traitType | workerid), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model2.1)$coef)

#model with trait type and condition * gender
model2.2 <- lmer(scale(rating) ~ traitType * condition * gender + (1 | trait) +(1 +traitType | workerid), data=d.b2)
kable(summary(model2.2)$coef)

#comparing the fit of the additive and interactive gender models
lrtest(model2.1 , model2.2)

#comparing the fit of the additive gender model with the model without gender
lrtest(model1.4 , model2.1)
```

Adding the interaction of gender will not improve the fit of the model, X2(3) = 1.8, p = 0.62. Therefore, to evaluate the effect of participants' gender on their rating of creativity, the additive model should be used. however, the additive gender model would not fit the data significantly better, X2(1) = 0.4, p = 0.53. Although the inclusion of gender did not improve the overall fit of the model, there is a pattern that should be flagged for future studies. The additive gender model suggests that male participants have marginally higher gender bias in attribution of creativity and they favor the masculine traits marginally more, b(male) = -0.19, t(81) = - 1.73, p = 0.09. Furthermore, including the gender of participants sligthly improves the significance of condition main effect, p=0.06 vs. 0.09. 


### Exploratory analyses 3: education effect

It might be the case that education can decrease the gender bias in attribution of creativity. To test this hypothesis, education level of participanst has been added to the fixed-effect model.

```{r Exploratory analyses 3: education effects}
#model with trait type and condition  + education
model3 <- lmer(rating ~ traitType * condition + educationL + (1 | trait) +(1 +traitType | workerid), data=d.b2)

#printing out the table of fixed-effect results
kable(summary(model3)$coef)


#comparing the fit of the additive gender model with the model without gender
lrtest(model1.4 , model3)
```

The education level of participants does not have a significant effect on participants' rating, t(81) = -1.53, p = 0.13. Furthermore, although including education in the model improved the fit significantly,X2(1) = 2220.7, p < 0.0001, this inclusion does not change significance of any previous variables. This result suggests that the increase in the education level of the participants neither decrease nor increase the gender bias in attribution of creativity. 

### Exploratory analyses 4: ethnicity effect

The other hypothesis is that participants with different ethnicity and cultural backgrounds might have different perceptions of creativity and consequently potentially different extent of gender bias in attribution of creativity. To test this hypothesis, the ethnicity of participants has been added to the mixed-effect model.

```{r Exploratory analyses 3:ethnicity effect}
#model with trait type and condition +  ethnicity
model4 <- lmer(rating~ traitType * condition + ethnicityF + (1 | trait) +(1 +traitType | workerid), data=d.b2)
#printing out the table of fixed-effect results
kable(summary(model4)$coef)

#comparing the fit of the additive gender model with the model without gender
lrtest(model1.4 , model4)
```

The gender bias in attribution of creativity does not differ according to the ethnicity of participants. However, the inclusion of ethnicity to the fixed effect model improves the fit of the model significantly, X2(4) = 2212.1, p < 0.0001. Also, by this inclusion the condition main effect is not significant anymore, t(76.53) = -1.59, p = 0.11. Therefore, it seems that when the variations across ethnicity are controlled, the effect of condition on the overall rating of the participants is not significant.



#Discussion

##I. Summary of Replication Attempt

The confirmatory analyses of this study show that there is a gender bias in attribution of creativity and participants perceive prototypically masculine traits more central to the creactivity. Furthermore, this bias can be manipulated by different conceptualizations of creativity. If a definition of creativity emphesizes more divergence from others and agency, this definition can marginally increase the gender biases and causing participants to favor the masculine traits more in regard of creativity attribution. However, use of the definition that challenges the gender bias and emphasizes converging to others and community would not cause the increase in the feminine rating that strongly. In short, the extent that a confirming signal could amplify a gender bias is higher than the extent a contradicting signal could decrease it. We can easily reinforce the bias, but cannot that easily refute it.


The study could be considered a partial replication of the original study as the main effect of trait type was significant in both the original studies and the replication study according to the mixed-model ANOVA anlysis. That said, it is partial not complete replication of the study due to following reasons that will be discussed. 


First, in the original study, the interaction between condition and trait type was marginally significant, however in the replication study this interaction was significant. Therefore, according to the original study the changes in the definition of the creativity can marginally influece the gender bias. However, according to the replication study this influence is significant. 

Second, the following effect sizes are smaller in the replication study than the original study: 1. the effect size for the ratings of masculine and feminine traits when using the divergent definition of creativity, Cohen's d(replication) = 1.02 vs. Cohen's d(original) = 1.25; 2. the effect size for the ratings of masculine and feminine traits when using convergent definition of creativity, Cohen's d(replication) = 0.64 vs. Cohen's d(original) = 0.94; 3. the effect size for the rating of masculine traits across the two different definitions of creativity, Cohen's d(replication) = 0.38 vs. Cohen's d(original) = 0.71. 

Third, for the third effect size not only the magnititude is smaller, but also the statistical significance is lower: in the orginal study, the ratings of masculine traits with diverging definition of creativity is significantly higher than those ratings with converging definition of creativity, p = 0.002, that said this difference was marginally significant for the replication study, p = 0.09. 

In some, the original and the replication study agree on that there is a significant gender bias in attribution of creativity. That said, they disagree to what extent this bias could be affected by a short manipulation in the definition of creativity.

##II. Commentary


It should be mentioned that the variance in the replication data was higher than the variance of the original study, standard deviations were higher in the replication study across conditions and trait types. This could explain why when the mixed-effect model was used to control for the random variance in the data, then the effect of condition on masculine traits' rating became significant. This resolves the above third mentioned points of discrepancies between the original and the replication study. As the data of the original study was not accessible, it is almost impossible to detect the source of this different random variances.

Although the effect of condition varied between the original and replication study, this replication can be considered a validation to the original study. The original paper is a collection of 5 studies with the driving main research question of whether there exists a gender bias in attribution of creativity. Both the original and the replication study came to same conclusion in regard of this central research question. There exists a gender bias in attribution of creativity and individuals perceive masculine traits more central to creativity. The partials disagreement of the original and the replication study was in the secondary research question: whether changing the definition of creativity can affect gender biases. Even for this question although the significance level of the data varied across the two studies, the pattern of the data, and the directionality of the effect was the same.

As it was recently mentioned in one of the many articles about replication crisis, "a replication is not a replica". In some cases, just due to mere statistics a finding would not be replicated, this does not mean the finding was not correct. Also, even if the significance of finding is confirmed, the effect size will not be exactly the same. In this replication, no evidence was found that the partial replication rather than the full replication was due to anything rather than the nature of statistics, and statistical findings. 

#Contacting the authors

The authors were contacted for asking about the exclusion policy as well as their materials for the first study. Shortly, the authors replied in regard of exclusion policy and generously provided the survey of the first study on the Qualtrics platform. In the original study, no participants were excluded. 

