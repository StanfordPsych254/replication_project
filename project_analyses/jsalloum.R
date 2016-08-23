library(tidyr)
library(dplyr)
library(rjson)

path <- "data/jsalloum/"
files = dir(path, pattern = "*.json")

d.raw = data.frame()

for (f in files) {
  jf = paste0(path, f)
  jd = fromJSON(paste(readLines(jf), collapse=""))
  for (elem in jd$answers$data) {
    newRow = data.frame(workerId = jd$WorkerId,
                        speakerId = elem$speakerId,
                        speakerSex = elem$sex,
                        plev = elem$plev,
                        behaviorScore = ifelse(is.null(elem$behaviorScore), NA,
                                               elem$behaviorScore))
    d.raw = bind_rows(d.raw, newRow)
  }
}

# d.af is the aggregated replication data 
d.af = d.raw %>% 
  mutate(workerId = as.factor(workerId),
        speakerId = as.factor(speakerId),
        speakerSex = ifelse(speakerSex == -1, "Male", "Female"),
        plev = ifelse(plev == -1, "Low", "High")) %>%
  group_by(speakerId, speakerSex, plev) %>%
  summarise(behaviorScore = mean(behaviorScore, na.rm = T))

# Additive model
rs1.1 = lm(behaviorScore ~ plev + speakerSex, data = d.af)

# Extract params
params = anova(rs1.1)
F_stat = params[1,4]
p = params[1,5]
dfnum = params$Df[1]
dfdenom = params$Df[length(params$Df)]
stat_descript <- paste0("F(",dfnum,", ",dfdenom,") = ",round(F_stat, 3))
stat_descript

## lsr::etaSquared(rs1.1) => d = 2.3758
#cohensD <- 2*sqrt(F_stat)/sqrt(dfdenom)
source("project_analyses/computeES.R")
es <- esComp(F_stat, df1 = dfnum, df2 = dfdenom, esType = "F")
es
project_info <- data.frame(
  project_key = "jsalloum", 
  rep_t_stat = sqrt(F_stat),
  rep_t_df = dfdenom,
  rep_final_n = length(d.af$speakerId), 
  rep_n_excluded = 0, 
  rep_es = es, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = p)


# Original plot

# Summary calculated from original data
##    plev speakerSex     mean        sem
##   (chr)      (chr)    (dbl)      (dbl)
## 1  High     Female 4.202099 0.06357308
## 2  High       Male 4.391540 0.07305057
## 3   Low     Female 3.424041 0.08737072
## 4   Low       Male 3.474154 0.12815603
plot_mean = c(4.20,4.39,3.42,3.47) 
plot_se = c(0.064,0.073,0.087,0.128)
plev = c("High Rank","High Rank","Low Rank","Low Rank")
speakerSex = c("Female Speaker","Male Speaker","Female Speaker","Male Speaker")

o_plot <- data.frame(plot_mean,plot_se,plev,speakerSex)
colnames(o_plot) <- c('mean', 'se', 'plev','speakerSex')
o_plot$plev <- factor(o_plot$plev,levels=c("Low Rank","High Rank"))
o_plot <- o_plot %>%
  mutate(upper = mean + se,
         lower = mean - se)

dodge <- position_dodge(width=0.9)

ggplot(o_plot, aes(x=plev, y = mean,fill = speakerSex)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(0,7.5)) + #to match original study
  xlab("Condition") +
  ylab("High-Rank Behavior") +
  ggtitle("Ko - Original") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position=c(.5,.8),
        legend.direction= "vertical",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"))

ggsave("figures/jsalloum-original.png",width = 1.5,height=1.5,units="in")

# Replication plot

se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

d_plot <- d.af %>% 
  group_by(plev,speakerSex) %>%
  summarise(mean = mean(as.numeric(behaviorScore),na.rm=T),
            se = se(as.numeric(behaviorScore)),
            upper = mean + se,
            lower = mean - se)
d_plot$speakerSex <- factor(d_plot$speakerSex)
levels(d_plot$speakerSex) <- list("Female Speaker"="Female","Male Speaker"="Male")
d_plot$plev <- factor(d_plot$plev)
levels(d_plot$plev) <- list("High Rank"="High","Low Rank"="Low")
d_plot$plev <- factor(d_plot$plev,levels=c("Low Rank","High Rank"))

ggplot(d_plot, aes(x=plev, y = mean,fill = speakerSex)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(0,7.5)) + #to match original study
  xlab("Condition") +
  ylab("High-Rank Behavior") +
  ggtitle("Ko - Replication") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position=c(.5,.8),
        legend.direction= "vertical",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"))

ggsave("figures/jsalloum-replication.png",width = 1.5,height=1.5,units="in")
