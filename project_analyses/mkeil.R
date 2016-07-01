library(tidyr)
library(rjson)
library(dplyr)

path <- "data/mkeil/"
files <- dir(path, pattern = "*.json")
d.raw <- data.frame()
for (f in files) {
  jf <- paste0(path, f)
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  # jd$answers$data[9]
  for(i in 1:8){
    id <- data.frame(workerId = jd$WorkerId,
                     trial_num = i,
                     start_num = jd$answers$data[[i]]$start_number,
                     digits = jd$answers$data[[i]]$digits,
                     word_condition = jd$answers$data[[i]]$word_condition,
                     save_choice = jd$answers$data[[i]]$save_choice,
                     save_condition = jd$answers$data[[i]]$save_condition,
                     FileB = jd$answers$data[[i]]$FileB,
                     FileA = jd$answers$data[[i]]$FileA,
                     B_Recall = jd$answers$data[[i]]$B_Recall,
                     A_Recall = jd$answers$data[[i]]$A_Recall,
                     time = jd$answers$data[[i]]$time,
                     age = jd$answers$data[[9]]$age,
                     gender = jd$answers$data[[9]]$gender,
                     homelang = jd$answers$data[[9]]$homelang,
                     race = jd$answers$data[[9]]$race
                     # comments = jd$answers$data[[9]]$comments
    )
    d.raw <- bind_rows(d.raw, id)
  }
}

####Score Digits

# This code calculates the number of times the subject correctly subtracted by 3 in the distractor task.
d1 <- d.raw

entered_digits = strsplit(d1$digits, "[\n]")
d1$DigitScore <- NA

for(i in seq(from=1, to=nrow(d1), by=1)){
  correct_digit = d1$start_num[i]
  digitScore = 0
  for(digit in entered_digits[[i]]){
    correct_digit = correct_digit -3
    if (digit == correct_digit){
      digitScore = digitScore + 1
    }
    correct_digit = as.numeric(digit)
  }
  
  d1$DigitScore[i] = digitScore
}

####Score FileRecall
# 
# This piece of code calculates the number of correct words recalled from File A and File B for each trial. The user input is compared to the actual list of words in the file, and for each match the score increases by 1. The code also gives participants credit for accidentally recalling the singular version of a plural word by translating it from singular to plural before scoring (examples: eggs, bikes, planes, and jeans). 

#FileA
FileA_split = strsplit(d1$FileA, "[,]")
FileA_recall_split = strsplit(d1$A_Recall, "[\n]")

d1$AScore <- NA
for(i in seq(from=1, to=nrow(d1), by=1)){
  FileA_split[[i]]
  FileAScore = 0
  for(word in FileA_recall_split[[i]]){
    #adjustment for plural words
    if(word == "egg"){
      word = paste(word,"s",sep = "")
    }
    word = tolower(c(word))
    if ( word %in% FileA_split[[i]]){
      FileAScore = FileAScore + 1
    }
  }
  if (d1$word_condition[i]==8){
    d1$AScore[i] = FileAScore/8
  }
  else {
    d1$AScore[i] = FileAScore/2
  }
}

d1$BScore <- NA
#FileB
for(i in seq(from=1, to=nrow(d1), by=1)){
  FileB_split = strsplit(d1$FileB, "[,]")
  FileB_split[[i]]
  FileB_recall_split = strsplit(d1$B_Recall, "[\n]")
  FileBScore = 0
  for(word in FileB_recall_split[[i]]){
    #adjustment for plural words
    if(word == "bike" | word == "plane" | word == "jean"){
      word = paste(word,"s",sep = "")
    }
    word = tolower(c(word))
    if ( word %in% FileB_split[[i]]){
      FileBScore = FileBScore + 1
    }
  }
  FileBScore
  d1$BScore[i] = FileBScore/8
}

####Data Exclusion
n_orig <- nrow(d1) # Save original datafile

#Remove first two trials(training) from analysis and exemption
#remove trialnum 1 and 2
d1 <- d1 %>% filter(trial_num != 1, trial_num != 2)


## Remove anyone who did not follow save instructions correctly

#rename save choice from delete
d1 <- d1 %>% mutate(save_condition = ifelse(save_condition == "nosave","delete","save"))

#remove wrong followers
included_workers <- d1 %>%
  group_by(workerId)  %>%
  summarise(followed_instructions = all(save_condition == save_choice)) %>%
  filter(followed_instructions)
d1 <- d1 %>% filter(workerId %in% included_workers$workerId)
#7 particpants eliminated


#Remove those 2 standard deviations below or above in word recall score and digit recall score.

d1_means <- d1 %>%group_by(workerId,word_condition) %>% summarise(meanB = mean(BScore), meanA =mean(AScore), meanDigit = mean(DigitScore), meanTime = mean(time))

#filter by recallScore
sb_meanB = sd(d1_means$meanB)
top_cutoff = mean(d1_means$meanB) + (2*sb_meanB)
bottom_cutoff = mean(d1_means$meanB) - (2*sb_meanB)

#filter by digitScore
sb_digitScore = sd(d1_means$meanDigit)
digit_top_cutoff = mean(d1_means$meanDigit) + (2*sb_digitScore)
digit_bottom_cutoff = mean(d1_means$meanDigit) - (2*sb_digitScore)

d1_keep <- filter(d1_means, meanB < top_cutoff & meanB > bottom_cutoff)
d1_keep <- filter(d1_keep, meanDigit > digit_bottom_cutoff)
d1_means <- d1_keep
d1 <- filter(d1, workerId %in% d1_keep$workerId)

# d1_bar <- d1 %>%
#   group_by(gender,race,age,workerId,word_condition,save_condition) %>% 
#   summarise(meanB = mean(BScore), 
#             meanA =mean(AScore), 
#             meanDigit = mean(DigitScore), 
#             meanTime = mean(time), 
#             seB = sem(BScore), 
#             seA = sem(AScore))

#47 included

# Save intermediate file
path <- "processed_data/"
write.csv(d1, file = paste0(path,"mkeil.csv")) #Check excel file for any issues with data

###Confirmatory Analyses

####Confirmatory Analysis 3: Mixed Design ANOVA for File B Performance (replication main analysis)

d1$word_condition = as.factor(d1$word_condition)
aov_f <- aov(BScore ~ save_condition*word_condition, data=d1)
aov_mixed <- aov(BScore ~ save_condition*word_condition + Error(workerId/save_condition), data=d1)
summary(aov_mixed) # double check this model?

# 
# The result of the original study 2x2 ANOVA:
# 
# > A 2 (trial type: save vs. no save) × 2 (condition: eight word vs. two word) mixed-design ANOVA revealed a significant interaction, F(1, 46) = 7.89, MSE = 0.01, p = .007, η2 = .15
# 
# Originally stated in replication report:
# The ANOVA did not show a significant interaction between save condition and word condition, F(1,46) = .206, MSE = .01, p = .65, η2 = .01. However, there was a significant main effect of save condition in the ANOVA, F(1,46) = 15.47, MSE = .50, p <.001, η2 = .25.


stat_sum <- summary(aov_mixed)$"Error: workerId:save_condition"
df1 <- stat_sum[[1]][2,"Df"]
df2 <- stat_sum[[1]][3,"Df"]
p.val <- round(stat_sum[[1]][2,"Pr(>F)"],2)
F_test<- round(stat_sum[[1]][2,"F value"],3)

stat_descript <- paste0("F(",df1,",",df2,") = ",F_test)



project_info <- data.frame(
  project_key = "mkeil", 
  rep_t_stat = sqrt(F_stat),
  rep_t_df = df2,
  rep_final_n = length(unique(d1$workerId)), 
  rep_n_excluded = length(unique(d.raw$workerId))-length(unique(d1$workerId)), 
  rep_es = NA, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = p.val,
  notes= "double check the aov mixed model- same as original authors? And different result than stated in report. This doesn't seem right collapsing by multiple trials first...?"
)
