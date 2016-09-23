library(dplyr)
library(tidyr)

data_location = "data/lampinen/"
files = list.files(path = data_location,pattern="data_subject_.*.json")
subject = vector()
trial_type=vector()
experiment_data=NULL
practice_trial=vector()
practice_trial=vector()
for (i in 1:length(files)) { 
  path = paste(data_location,files[i],sep="") #Path to file
  c = file(path, "r")
  l = readLines(c, -1L)
  close(c)
  these_data = lapply(X=l, rjson::fromJSON) #Convert list of json objects to list of R objects
  
  this_trial_data = list();
  j = 1;
  #For each trial in this experiment, extract data from it
  for (trial_i in 1:length(these_data[[1]])) {
    if(grepl("rotation",gsub(" ","",these_data[[1]][[trial_i]]$trial_type))) { #Only get data from rotation trials for this analysis, don't care about instructions, etc.
      this_trial_data[[j]] = these_data[[1]][[trial_i]]
      for (data_i in 1:length(this_trial_data[[j]])) {
        if (length(this_trial_data[[j]][[data_i]]) > 1) { #Handle lists contained within a single trial's data
          this_trial_data[[j]][[data_i]] = paste(this_trial_data[[j]][[data_i]],collapse='')
        }
      }
      j = j+1
    }
  }
  this_trial_data = do.call(bind_rows, lapply(this_trial_data, data.frame)); #convert to data frame
  this_trial_data$subject = i #Anonymous subject ids
  if (is.null(experiment_data)) {
    experiment_data = bind_rows(this_trial_data)
  }
  else {
    experiment_data = bind_rows(experiment_data,this_trial_data)
  }
}

experiment_data = experiment_data %>% 
  mutate(choiceStr = as.character(response_choices)) %>%
  mutate(correct_response_key = ifelse(substr(as.character(experiment_data$response_mappings),1,7) == 'correct',
                                       as.numeric(substr(choiceStr,1,2)),
                                       as.numeric(substr(choiceStr,3,4))),
         rotating = (rotation_speed != 0),
         experiment_trial_type=gsub(" ","",experiment_trial_type)) %>%
  mutate(answer_was_correct=ifelse(correct_response == "correct",
                                   response == correct_response_key,
                                   response != correct_response_key))

# Find which subjects passed more than 75% of the consonant checks, 
# exclude those who didn't
filtering_data = experiment_data %>% 
  filter(verb_supp_check) %>% 
  group_by(subject) %>% summarise(score=sum(consonant_correct_count==4)/n()) %>% 
  mutate(accept = score >= 0.4) 

subjects_to_keep = filtering_data[filtering_data$accept,]$subject
filtered_data = experiment_data %>% filter((subject %in% subjects_to_keep) & !practice_trial)

# coefficients for calculating capacity K
a = -1
b = 2*4+1 

subject_aggregated_data = filtered_data %>% 
  group_by(subject,experiment_trial_type,rotating) %>%
  summarise(percent_correct = sum(answer_was_correct)/n()) %>% 
  spread(experiment_trial_type,percent_correct) %>% 
  mutate(K1 = (-b+sqrt(b^2-4*a*(-2*6*(swap-(1-correct)))))/(2*a),
         K2 = (-b-sqrt(b^2-4*a*(-2*6*(swap-(1-correct)))))/(2*a)) %>% 
  mutate(K = K1) 

# Write tidied data to file
path <- "processed_data/"
write.csv(subject_aggregated_data, file = paste0(path,"lampinen.csv"),
          row.names = F, quote = F) 

n_subjects = length(unique(subject_aggregated_data$subject))
nonRotatingK = subject_aggregated_data[!subject_aggregated_data$rotating,]$K
rotatingK = subject_aggregated_data[subject_aggregated_data$rotating,]$K
t_res = t.test(nonRotatingK, rotatingK, paired=T)

F_stat = (t_res$statistic)^2
p = pf(F_stat,1,n_subjects-1,lower.tail=F)
stat_descript <- paste0("F(",1,", ",t_res$parameter,") = ",round(F_stat, 3))

source("project_analyses/computeES.R")
es <- esComp(F_stat, df1 = 1, df2 = t_res$parameter, esType = "F")

project_info <- data.frame(
  project_key = "lampinen", 
  rep_t_stat = t_res$statistic,
  rep_t_df = t_res$parameter,
  rep_final_n = n_subjects, 
  rep_n_excluded = length(files) - n_subjects, 
  rep_es = es, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = p)

# Original plot
se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

plot_mean = c(0.96,1.86)
plot_se = c(0.1,0.15)
plot_rotating = c("Rotating","Static")
o_plot <- data.frame(plot_mean,plot_se,plot_rotating)
colnames(o_plot) <- c('mean', 'se', 'rotating')
o_plot <- o_plot %>%
  mutate(upper = mean + se,
         lower = mean - se)

ggplot(o_plot, aes(x=rotating, y = mean,fill = rotating)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3) +
  coord_cartesian(ylim=c(0,2.5)) + #to match original study
  xlab("Condition") +
  ylab("Feature-Part Capacity (K)") +
  ggtitle("Xu - Original") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position="none")

ggsave("figures/lampinen-original.png",width = 1.5,height=1.5,units="in")

# Replication plot

d_plot <- subject_aggregated_data %>% 
  group_by(rotating) %>%
  summarise(mean = mean(as.numeric(K),na.rm=T),
            se = se(as.numeric(K)),
            upper = mean + se,
            lower = mean - se)
d_plot$rotating <- c("Static","Rotating")

ggplot(d_plot, aes(x=rotating, y = mean,fill = rotating)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3) +
  coord_cartesian(ylim=c(0,2.5)) + #to match original study
  xlab("Condition") +
  ylab("Feature-Part Capacity (K)") +
  ggtitle("Xu - Replication") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position="none")

ggsave("figures/lampinen-replication.png",width = 1.5,height=1.5,units="in")

