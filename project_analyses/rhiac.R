# Replication of "How Will I Be Remembered? Conserving the Environment for the Sake of One's Legacy" by Zaval, Markowitz & Weber (2015, Psychological Science)
# Rhia Catapano

# MCF NOTE: this script runs over hand-deduped anonymized data. I removed by hand
# the 7 datapoints that were accidentally run twice (all in the control
# condition). There should be 321 participants (not 328).


library(tidyr) 
library(dplyr)
library(ggplot2) 
library(rjson)

# Therefore, I've run power analysis on one of the key effects, 
# the effect of condition on behavioral intentions (d=0.36)

path <- "data/rhiac/"
files <- dir(path, 
             pattern = "*.json")
d.raw <- data.frame(stringsAsFactors = FALSE)

for (f in files) {
  jf <- paste0(path, f)
  jd <- fromJSON(jf)
  
  #get rid of all of the instruction trials, which are labeled with 0
  cols <- which(jd$answers$data$trial_number_block != 0)
  
  #This code formats everything into a useable dataframe, and accounts for missing values by turning them into NA's
  df <- jd$answers$data
  
  id <- data.frame(workerid = jd$WorkerId, 
                   totalTime = df$totalTime,
                   totalHiddenTime = ifelse(is.null(unlist(df$totalHiddenTime)),NA,
                                            unlist(df$totalHiddenTime)),
                   condition = df$condition,
                   trialNum = df$trial_number_block[cols],
                   trialType = df$trial_type[cols],
                   sentence = df$sentence[cols],
                   rating = df$rating, 
                   environmentalist =
                     ifelse(is.null(unlist(df$environmentalist)), NA,
                            unlist(df$environmentalist)),
                   legacy_essay = df$legacy_essay,
                   sex = 
                     ifelse(is.null(unlist(df$sex)), NA, 
                            unlist(df$sex)), 
                   race = 
                     ifelse(is.null(unlist(df$race)), NA,
                            unlist(df$race)),
                   year = df$year,
                   english =
                     ifelse(is.null(unlist(df$english)),NA,
                            unlist(df$english)),
                   grandparent =
                     ifelse(is.null(unlist(df$grandparent)),NA,
                            unlist(df$grandparent)),
                   children = 
                     ifelse(is.null(unlist(df$children)),NA,
                            unlist(df$children)),
                   stringsAsFactors=FALSE)
  
  d.raw <- bind_rows(d.raw, id)
}

#remove "filler"" questions
fillers <- c("I am well liked by my friends", 
             "I feel a connection to future generations",
             "I feel a sense of responsibility to future generations",
             "I have important skills I can pass along to others",
             "Others would say I have made unique contributions to my community or society")
d.raw <- d.raw %>% filter(!sentence %in% fillers)

# The 3 DV's are ratings on the behavioral intentions trials, ratings on the 
# environmental attitudes trials, and ratings on the legacy motives trials. The 
# indexes for each are computed as the average of the ratings for the items that 
# make up the composite index.

trial_index <- d.raw %>%
 group_by(workerid, trialType) %>%
 summarize("index"=mean(as.numeric(rating)), "condition" = condition[1], "year"=year[1], "sex"=sex[1], "environmentalist" = environmentalist[1], "race" = race[1], "year" = year[1], "grandparent" = grandparent[1], "children" = children[1], leg_essay=legacy_essay[1])

d <- spread(trial_index, trialType, index)
d <- d %>%
  rename(beh_int_index = beh_int_trial,
         env_att_index = env_att,
         leg_mot_index = legacy_motives2)


#Also correctly factor/numeric variables as appropriate
d$condition <- factor(d$condition, levels = c(0, 1), labels = c("control", "leg_prime"))
d$sex <- as.factor(d$sex)
d$environmentalist <- as.factor(d$environmentalist)
d$race <- as.factor(d$race)
d$grandparent <- as.factor(d$grandparent)
d$children <- as.factor(x = d$children)
d$year <- as.numeric(d$year)

write_csv(d, "processed_data/rhiac.csv")

# Key analysis
model <- lm(beh_int_index ~ condition, d)
kable(tidy(model))



**Key Effects from Paper**
  
  Effect of legacy prime on legacy motives

The original paper reports that, "As expected, the essay manipulation successfully enhanced overall legacy motives. Almost 10 min after being exposed to the manipulation, participants who wrote the essay reported higher legacy motives (M = 4.47, SD = 1.06), compared with those in the control condition (M = 4.19, SD = 1.05), F(1, 310) = 5.64, p = .018, Cohen’s d = 0.27."

```{r, message=FALSE, warning=FALSE}
model <- lm(leg_mot_index ~ condition, d)
kable(tidy(model))
```

Effect of legacy prime on environmental behavior intentions

The original paper reports: "We also found a significant effect of the legacy prime on behavioral intentions. Participants who were primed reported greater behavioral intentions (M = 3.05, SD = 0.86) than those who were not primed (M=2.73,SD=0.85), F(1,309)=10.07, p=.002, d=0.36."

```{r, message=FALSE, warning=FALSE}
```

Effect of legacy prime on beliefs/attitudes

The original paper reports: "Moreover, as predicted, we found a significant effect of the legacy prime on willingness to engage in behaviors aimed at combating climate change. Participants who were primed with legacy motives had higher belief scores (M = 5.39, SD = 1.08) than those in the control condition (M = 5.11, SD = 1.27), F(1, 309) = 4.08, p = .040, d = 0.23."

```{r, message=FALSE, warning=FALSE}
model <- lm(env_att_index ~ condition, d)
kable(tidy(model))
```



**Mediation Analysis**
  
  ```{r, warning=FALSE, message=FALSE, error=FALSE, echo=FALSE}
library(multilevel) #contains sobel
#printing sobel
sob_print <- function(sob){
  df <- do.call(rbind, sob)
  rnames <- rownames(df)
  rnames[c(1,3,6)] <- c("Mod1: Y~X", "Mod2: Y~X+M", "Mod3: M~X")
  rownames(df) <- rnames
  
  return(list("Models"=df[1:7,],
              "Vals"=data.frame("IndirectEffect"=df[8,1],
                                "SE"=df[9,1],
                                "Z"=df[10,1],
                                "N"=df[11,1])))
}
```


**Path 1: Prime -> Legacy Motives -> Attitudes**
  
  The original paper reports: "To confirm that the influence of the legacy prime on climate-change attitudes was driven by increases in legacy motives, we conducted a mediation analysis using the mean legacy-motives score as a mediator. The effect of the legacy prime on individuals’ climate-change beliefs was fully mediated by increases in legacy motives—direct effect of the priming condition on beliefs: t(308) = 2.01; effect of priming condition on beliefs, mediated by legacy motives: t(308) = 1.29, Sobel’s z = 2.19, p = .028. Mediation was also confirmed by a bias-corrected bootstrapping procedure using 5,000 samples (Preacher & Hayes, 2008; see Median Test in the Supplemental Material). This analysis showed that the indirect effect of the legacy prime on climate-change beliefs through legacy motives was significant, with a 95% confidence interval (CI) that excluded zero (β = 0.041, 95% CI = [0.01, 0.09], p = .029). "

Sobel
```{r, warning=FALSE, message=FALSE, error=FALSE}
sobel = sobel(d$condition, d$leg_mot_index, d$env_att_index)
sobel_chart <- sob_print(sobel)
kable(sobel_chart$Models)
kable(sobel_chart$Vals)
```

Bootstrapping
```{r, warning=FALSE, message=FALSE, error=FALSE}
mediation(x = as.numeric(d$condition), 
          mediator = d$leg_mot_index, 
          dv = d$env_att_index, 
          conf.level=.95, bootstrap=TRUE, B=5000)
```


**Path 2: Prime -> Legacy Motives -> Behavioral Intentions**
  
  The original paper reports: "When we replaced climate-change attitudes with behavioral intentions as the outcome variable, behavioral intentions were also partially mediated by increases in legacy motives—direct effect of priming condition on intentions: t(311) = 3.17, effect of priming condition on beliefs, mediated by leg- acy motives: t(311) = 2.65, Sobel’s z = 2.11, p = .034; the indirect effect was significant (β = 0.034, 95% CI = [0.01, 0.08], p = .035)."

Sobel
```{r, warning=FALSE, message=FALSE, error=FALSE}
sobel = sobel(d$condition, d$leg_mot_index, d$beh_int_index)
sobel_chart <- sob_print(sobel)
kable(sobel_chart$Models)
kable(sobel_chart$Vals)
```

Bootstrapping
```{r, warning=FALSE, message=FALSE, error=FALSE}
mediation(x = as.numeric(d$condition), 
          mediator = d$leg_mot_index, 
          dv = d$beh_int_index, 
          conf.level=.95, bootstrap=TRUE, B=5000)
```


###Exploratory analyses

In the origial study, participants spent an average of 6.5 minutes writing their legacy essays. However, in the current study, participants spent approximately 4 minutes writing their legacy essays. Therefore, one major difference between the two versions may be the strength of the manipulation.

To further examine the manipulation, we can look at the relationship between essay length and legacy motives, as well as the DV's of interest. (We can only make these comparisons for the legacy prime condition, given that the other condition did not complete an essay, and is not a reasonable basis for comparison).

```{r}
d$wordcount <- word_count(d$leg_essay)
legacy <- filter(d, condition == "leg_prime")

model <- lm(leg_mot_index ~ wordcount, d)
kable(tidy(model))

model <- lm(env_att_index ~ wordcount, d)
kable(tidy(model))

model <- lm(beh_int_index ~ wordcount, d)
kable(tidy(model))
```

Although word count doesn't seem to affect legacy motives, it does predict environmental attitudes (t = 2.07, p = 0.04) and marginally predicts behavioral intentions (t = 1.9, p = 0.06). This finding suggests that the shortening of the manipulation may be a major contributing factor in the parts of the study that did not replicate.

##Discussion

###Summary of Replication Attempt The current project partially replicated the
#findings of Zaval et. al. Specifically, we replicated the result that a short
#writing task about legacy motives increases both individuals' legacy motives,
#and increases their behavioral intentions for environmental behaviors. However,
#we were not able to replicate their finding that this manipulation affects
#environmental attitudes.

#In addition, the current project replicated both of the mediation analyses
#presented in Zaval et. al., showing that legacy condition affects legacy
#motives, which in turn affects environmental behaviors and attitudes (even
#though we weren't able to replicate the environmental attitudes result).

#Finally, the broad trends present in the data also support the Zaval et. al. findings. Specifically, we see that as legacy motives increase, both environmental behavioral intentions, and environmental attitudes. In addition, we see that the manipulation used does affect legacy motives. Therefore, our findings are fully congruent with the reported findings, despite the parts we were unable to replicate.

###Commentary
#One major insight from this replication is the importance of time spent on the manipulation. In the original study, participants spent an average of 6.5 minutes on the legacy motives writing task. However, in the current study, participants spent only 4 minutes on the writing task, due to budgetary contraints. This difference is likely a contributing factor for our inability to replicate some of their effects. The exploratory analysis indicates that for the legacy condition, number of words written predicts both environmental attitudes and behavioral intentions. This suggests that writing more likely makes the manipulation stronger, and therefore, did affect our results negatively.