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

full_cohensDVal <- lsr::cohensD((epoch %>% filter(Transition == "Fade"))$meanTime, 
                                (epoch %>% filter(Transition == "Slide"))$meanTime)

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
  rep_t_stat = full_t.test$statistic,
  rep_t_df = full_t.test$parameter,
  rep_final_n = length(unique(d$WorkerID)), 
  rep_n_excluded = 0, 
  rep_es = full_cohensDVal, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = full_t.test$p.value)


### PLOTS
se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

ms <- epoch %>%
  group_by(epoch, Transition) %>%
  summarise(se = se(meanTime)/1000, 
            time = mean(meanTime)/1000)

ggplot(ms, aes(x=epoch, y = time, col = Transition, group=Transition)) + 
  geom_line() + 
  geom_point(position = position_dodge(width = .05)) + 
  geom_errorbar(aes(ymin=time-se, ymax=time+se),
                size =.3,
                width =.3, 
                position = position_dodge(width = .05))  + 
  geom_label_repel(data = filter(ms, epoch==2), 
                   aes(label = Transition), size = 2) + 
  theme_bw(base_size = 6) +
  scale_colour_grey(start=.4, guide=FALSE) + 
  ggtitle("Liverence - Replication") + 
  xlab("Epoch") + 
  scale_y_continuous(name = "Time (s)", limits=c(5,18))

ggsave("figures/jedtan-replication.png",width = 1.5, height=1.5, units="in")


## Original
# estimated from figure
original <- tibble(epoch = factor(c(1:5, 1:5)), 
                       condition = factor(c(rep("Fade",5),
                                     rep("Slide",5))),
                       time = c(13.8,11.2,10.1,10.3,10.1,
                                11.9,9.5,9.2,9.1,8.7),
                       se = c(1,.7,.7,.7,.7,
                              .5, .4,.4,.4,.4))
                       
ggplot(original, aes(x=epoch, y = time, col = condition, group=condition)) + 
  geom_line() + 
  geom_point(position = position_dodge(width = .05)) + 
  geom_errorbar(aes(ymin=time-se, ymax=time+se),
                size =.3,
                width =.3, 
                position = position_dodge(width = .05))  + 
  geom_label_repel(data = filter(original, epoch==2), 
                   aes(label = condition), size = 2) + 
  theme_bw(base_size = 6) +
  scale_colour_grey(start=.4, guide=FALSE) + 
  ggtitle("Liverence - Original") + 
  xlab("Epoch") + 
  scale_y_continuous(name = "Time (s)", limits=c(5,18)) 

ggsave("figures/jedtan-original.png",width = 1.5, height=1.5, units="in")

