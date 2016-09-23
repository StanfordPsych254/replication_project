# Replication of "How Will I Be Remembered? Conserving the Environment for the Sake of One's Legacy" by Zaval, Markowitz & Weber (2015, Psychological Science)
# Rhia Catapano

library(tidyr) 
library(dplyr)
library(ggplot2) 

path <- "data/rhiac/"
files <- dir(path, 
             pattern = "*.json")
d.raw <- data.frame(stringsAsFactors = FALSE)

for (f in files) {
  jf <- paste0(path, f)
  jd <- jsonlite::fromJSON(jf)

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
modres = anova(model)
Fstat1 = modres[1,4] #time metric
dfnum1 = modres[1,1]
dfdenom1 = modres$Df[2]
pval1 = modres[1,5]

stat_descript <- paste0("F(",dfnum1,", ",dfdenom1,") = ",round(Fstat1, 3))

source("project_analyses/computeES.R")
es <- esComp(Fstat1, df1 = dfnum1, df2 = dfdenom1, esType = "F")

project_info <- data.frame(
  project_key = "rhiac", 
  rep_t_stat = sqrt(Fstat1),
  rep_t_df = dfdenom1,
  rep_final_n = 321, 
  rep_n_excluded = 7, 
  rep_es = es, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = pval1)

# Original Plot
se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

# Original plot
plot_mean = c(2.73,3.05) 
plot_se = c(0.86,0.85) / sqrt(310)
condition = c("Control","Essay Prime")
o_plot <- data.frame(plot_mean,plot_se,condition)
colnames(o_plot) <- c('mean', 'se', 'condition')
o_plot <- o_plot %>%
  mutate(upper = mean + se,
         lower = mean - se)

ggplot(o_plot, aes(x=condition, y = mean,fill = condition)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3) +
  coord_cartesian(ylim=c(0,4)) + # 1.2 - 6 matches original study but looks terrible
  xlab("Condition") +
  ylab("Behavioral Intentions") +
  ggtitle("Zaval - Original") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=1:6)

ggsave("figures/rhiac-original.png",width = 1.5,height=1.5,units="in")


# Replication Plot
d_plot <- d %>% 
  group_by(condition) %>%
  summarise(mean = mean(as.numeric(beh_int_index),na.rm=T),
            se = se(as.numeric(beh_int_index)),
            upper = mean + se,
            lower = mean - se)
d_plot$condition <- c("Control","Essay Prime")

ggplot(d_plot, aes(x=condition, y = mean,fill = condition)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3) +
  coord_cartesian(ylim=c(0,4)) + 
  xlab("Condition") +
  ylab("Behavioral Intentions") +
  ggtitle("Zaval - Replication") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=1:6)

ggsave("figures/rhiac-replication.png",width = 1.5,height=1.5,units="in")
# 
# ggplot(ms_beh, aes(x=condition, y=meanBeh)) + 
#   geom_bar(stat = "identity") + 
#   ylim(0, 6) +
#   geom_errorbar(aes(ymin = meanBeh - ci95, 
#                     ymax = meanBeh + ci95), width=0.25)


# model <- lm(leg_mot_index ~ condition, d)
# summary(model)
# model <- lm(env_att_index ~ condition, d)
# summary(model)

#printing sobel
# sob_print <- function(sob){
#   df <- do.call(rbind, sob)
#   rnames <- rownames(df)
#   rnames[c(1,3,6)] <- c("Mod1: Y~X", "Mod2: Y~X+M", "Mod3: M~X")
#   rownames(df) <- rnames
#   
#   return(list("Models"=df[1:7,],
#               "Vals"=data.frame("IndirectEffect"=df[8,1],
#                                 "SE"=df[9,1],
#                                 "Z"=df[10,1],
#                                 "N"=df[11,1])))
# }
# 
# sobel = sobel(d$condition, d$leg_mot_index, d$env_att_index)
# sobel_chart <- sob_print(sobel)
# 
# mediation(x = as.numeric(d$condition), 
#           mediator = d$leg_mot_index, 
#           dv = d$env_att_index, 
#           conf.level=.95, bootstrap=TRUE, B=5000)
# 
# sobel = sobel(d$condition, d$leg_mot_index, d$beh_int_index)
# sobel_chart <- sob_print(sobel)
# 
# mediation(x = as.numeric(d$condition), 
#           mediator = d$leg_mot_index, 
#           dv = d$beh_int_index, 
#           conf.level=.95, bootstrap=TRUE, B=5000)
# 
