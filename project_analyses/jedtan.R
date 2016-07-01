library(dplyr)
# library(ggplot2)
# library(lsr)
# library(lme4)

rawD <- read.csv("data/jedtan/final_results_anon.csv")
rawDem <- read.csv("data/jedtan/final_demographics_anon.csv")

d <- rawD %>%
  filter(Time < 30000, Trial > 2, Block > 0) %>% 
  mutate(epoch= floor((Trial-1)/10) + 1, keyPressesOver = KeyPresses-MinKeyPresses) %>% 
  rename(Transition=Stimulus)
d$Transition <- factor(d$Transition, levels=c(0, 1), labels=c("Slide", "Fade"))
d$epoch <- factor(d$epoch)

transition_means = d %>% 
  group_by(WorkerID, Transition) %>% 
  summarise(meanTime = mean(Time), meanKeyPressOver = mean(keyPressesOver)) %>% 
  group_by(Transition) %>% 
  summarise(meanTime = mean(meanTime), meanKeyPressOver = mean(meanKeyPressOver))
transition_worker_means = d %>% 
  group_by(WorkerID, Transition, Age) %>% 
  summarise(meanTime = mean(Time), meanKeyPressOver = mean(keyPressesOver))

slide_mean = round(transition_means$meanTime[1]/1000, digits=2)
fade_mean =  round(transition_means$meanTime[2]/1000, digits=2)

full_t.test <- t.test((transition_worker_means %>% filter(Transition == "Fade"))$meanTime, (transition_worker_means %>% filter(Transition == "Slide"))$meanTime, paired=TRUE)

epoch <- d %>% group_by(WorkerID, epoch, Transition) %>% 
 summarise(meanTime = mean(Time), meanPress = mean(KeyPresses))

full_cohensDVal <- lsr::cohensD((epoch %>% filter(Transition == "Fade"))$meanTime, (epoch %>% filter(Transition == "Slide"))$meanTime)

#As a key statistic, Liverence and Scholl report that "Across all trials, 
# participants were on average 1.41 s faster per trial on slide-transition blocks
# than on fade-transition blocks (9.27 s vs. 10.68 s, a difference of 15.2%), 
#t(17) = 5.22, p< .001, d = 1.23." The same statistics generated over all trials 
#in the new condition found that participants were on average 
#`r fade_mean-slide_mean`s faster per trial on slide-transition blocks than on 
#fade-transition blocks (`r slide_mean`s vs. `r fade_mean`s, a difference of 
#`r round(((slide_mean / fade_mean) - 1)*-100, digits=2)`%), 
# t(18) = `r round(full_t.test$statistic, digits=2)`, 
# p= `r round(full_t.test$p.value, digits=2)`, 
# d=`r round(full_cohensDVal, digits=2)`. 

stat_descript = paste0("t(",full_t.test$parameter,")=",round(full_t.test$statistic,digits=3))

project_info <- data.frame(
  project_key = "jedtan", 
  rep_final_n = length(unique(d$WorkerID)), 
  rep_n_excluded = 0, 
  rep_es = full_cohensDVal, 
  rep_test_statistic = stat_descript,
  rep_p_value = full_t.test$p.value)

# 
# time_differences <- d %>%
#   group_by(epoch) %>% 
#   summarise(meanTime = mean(Time))
# 
# 
# overTrial <- d %>% group_by(Trial, Transition, epoch) %>% 
#   summarise(meanTime = t.test(Time)$estimate, timeLow = t.test(Time)$conf.int[1], timeHigh = t.test(Time)$conf.int[2], meanPress = t.test(KeyPresses)$estimate, meanLow = t.test(KeyPresses)$conf.int[1], meanHigh = t.test(KeyPresses)$conf.int[2], meanKeyPressOver = t.test(keyPressesOver)$estimate, overLow = t.test(keyPressesOver)$conf.int[1], overHigh = t.test(keyPressesOver)$conf.int[2])
# totalMeans <- d %>% group_by(Transition, epoch) %>% 
#   summarise(meanTime = t.test(Time)$estimate, timeLow = t.test(Time)$conf.int[1], timeHigh = t.test(Time)$conf.int[2], meanPress = t.test(KeyPresses)$estimate, meanLow = t.test(KeyPresses)$conf.int[1], meanHigh = t.test(KeyPresses)$conf.int[2], meanKeyPressOver = t.test(keyPressesOver)$estimate, overLow = t.test(keyPressesOver)$conf.int[1], overHigh = t.test(keyPressesOver)$conf.int[2])
# 
# test_statistics <- data.frame(Epoch=c(1,2,3,4,5)) %>% 
#   mutate(Statistic = 0, PValue = 0)
# 
# for(i in 1:5)
# {
#   temp_t.test = t.test((epoch %>% filter(epoch == i, Transition == "Fade"))$meanTime, (epoch %>% filter(epoch == i, Transition == "Slide"))$meanTime, paired=TRUE)
#   test_statistics[i, 2] = temp_t.test$statistic
#   test_statistics[i, 3] = temp_t.test$p.value
# }
# test_statistics
# 
# 
# 
# ggplot(data=totalMeans, aes(x=epoch, y=meanTime, group = Transition, colour = Transition)) +
#   geom_line() +
#   geom_errorbar(aes(ymin=timeLow, ymax=timeHigh), width=.2) + 
#   geom_point( size=2, shape=21, fill="white") + labs(title = "Per Epoch Means") + xlab("Epoch") + ylab("Mean Time (ms)")
# 
# 
# ggplot(data=overTrial, aes(x=Trial, y=meanTime, group = Transition, colour = Transition)) +
#   geom_line() +
#   geom_errorbar(aes(ymin=timeLow, ymax=timeHigh), width=.2) + 
#   geom_point( size=2, shape=21, fill="white") + labs(title = "Per Trial Means") + xlab("Trial") + ylab("Mean Time (ms)")
# 
# 
