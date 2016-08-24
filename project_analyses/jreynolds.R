library(tidyr)
library(dplyr)
library(ggplot2)

#function to get confidence interval
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-lmtest::coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}

#function to compute clustered SE, run regression using clustered SE and return coefficients and confidence intervals
super.cluster.fun<-function(model, cluster) {
  vcovCL<-multiwayvcov::cluster.vcov(model, cluster)
  
  assign("coef", lmtest::coeftest(model, vcovCL),.GlobalEnv)
  #coef<-coeftest(model, vcovCL)
  w<-lmtest::waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  coefs=coef
  cis=ci
  
  return(list(coef, w, ci))
}

less_more<-function(a,b) {
  ifelse(a>b,"more","less")
}

d <- read.csv("data/jreynolds/data.csv",na.strings=c("","NA"))
d$Participant<-factor(d$Participant)
d$Evaluation.Type<-relevel(d$Evaluation.Type,"Predicted")
d$Evaluation.Contrast<-d$Evaluation.Type
contrasts(d$Evaluation.Contrast)=c(1,-1)
d$Condition.Contrast<-d$Condition
contrasts(d$Condition.Contrast)<-c(-1,1)

m1<-lm(Liking~Condition.Contrast*Evaluation.Contrast,data=d)#standard OLS regression
rs1<-super.cluster.fun(m1, d$Participant)

interactionRow = rs1[[1]][4,]
tval = interactionRow[3]
pval = interactionRow[4]
df = rs1[[2]][1,]$Res.Df

cohensd <- 2*tval/sqrt(df)

stat_descript <- paste0("t(",df,") = ",round(tval, 3))

project_info <- data.frame(
  project_key = "jreynolds", 
  rep_t_stat = tval,
  rep_t_df = df,
  rep_final_n = length(unique(d$Participant)), 
  rep_n_excluded = 0, 
  rep_es = cohensd, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = pval)

# Original plot

se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

plot_mean = c(4.85,4.52,5.28,4.31) # From Paper
plot_se = c(.257,.073,.257,.073) # From Paper
Evaluation.Contrast = c("Predicted","Actual","Predicted","Actual")
Condition.Contrast <- c("Control","Control","Maximize","Maximize")

o_plot <- data.frame(plot_mean,plot_se,Evaluation.Contrast,Condition.Contrast)
colnames(o_plot) <- c('mean', 'se', 'Evaluation.Contrast','Condition.Contrast')
o_plot$Evaluation.Contrast <- factor(o_plot$Evaluation.Contrast, levels=c("Predicted","Actual"))

o_plot <- o_plot %>%
  mutate(upper = mean + se,
         lower = mean - se)

dodge <- position_dodge(width=0.9)
ggplot(o_plot, aes(x=Condition.Contrast, y = mean,fill = Evaluation.Contrast)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(2.9,6.5)) + #to match original study
  xlab("Instruction") +
  ylab("Liking") +
  ggtitle("Scopelliti - Original") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position=c(.5,.85),
        legend.direction= "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"),
        plot.title = element_text(color="#dc322f"))

ggsave("figures/jreynolds-original.png",width = 1.5,height=1.5,units="in")

# Replication plot
d_plot <- d %>% 
  group_by(Evaluation.Contrast,Condition.Contrast) %>%
  summarise(mean = mean(as.numeric(Liking),na.rm=T),
            se = se(as.numeric(Liking)),
            upper = mean + se,
            lower = mean - se)

ggplot(d_plot, aes(x=Condition.Contrast, y = mean,fill = Evaluation.Contrast)) + 
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                size=.3,
                width=.3,
                position=dodge) +
  coord_cartesian(ylim=c(2.9,6.5)) + #to match original study
  xlab("Instruction") +
  ylab("Liking") +
  ggtitle("Scopelliti - Replication") +
  theme_bw(base_size = 6) +
  scale_fill_grey(start=.4) +
  theme(legend.position=c(.5,.85),
        legend.direction= "horizontal",
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text=element_text(size=4),
        legend.key.size = unit(3, "mm"),
        plot.title = element_text(color="#dc322f"))

ggsave("figures/jreynolds-replication.png",width = 1.5,height=1.5,units="in")
