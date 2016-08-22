library(ggplot2)
library(tidyr)
library(dplyr)

##Results

# Pre-data processing
# d_id <- read.csv("~/Downloads/Batch_2303798_batch_results.csv")
# d_id$AssignmentId <- paste0("anon-",row.names(d_id))
# write.csv(d_id,"~/Downloads/ehermann.csv",row.names=F)

# Read in this data
path <- "processed_data/"
df <- read.csv(file = paste0(path,"ehermann.csv")) #Check excel file for any issues with data

df <- df %>% rename(
  pfClaimScore=Answer.personalFinanceClaimScore,
  overclaimScore=Answer.overclaimingScore,
  flQuizScore=Answer.FLQuizScore, accuracy=Answer.accuracy,
  pfFirst=Answer.personalFinanceFirst)

personalFinanceFirstData <- df[df$pfFirst == 1,]
itemQuestionsFirstData <- df[df$pfFirst == 0,]

###Confirmatory analysis
# The analyses the authors did were as follows:

data.lm = lm(formula = scale(overclaimScore) ~ scale(pfClaimScore) + scale(accuracy), data = df)

pval <- summary(data.lm)$coef["scale(pfClaimScore)","Pr(>|t|)"]
tval <- round(summary(data.lm)$coef["scale(pfClaimScore)","t value"],3)
est <- round(summary(data.lm)$coef["scale(pfClaimScore)","Estimate"],3)
degrees <- data.lm$df

stat_descript <- paste0("t(",degrees,") = ",tval)

cohensd <- 2 * tval / sqrt(degrees)

project_info <- data.frame(
  project_key = "ehermann", 
  rep_t_stat = tval,
  rep_t_df = degrees,
  rep_final_n = nrow(df), 
  rep_n_excluded = 0 , 
  rep_es = cohensd, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = pval,
  notes= ""
)

# Original plot
data_slope <- 0.09
data_intercept <- .305 - (4.43 * .09)# get intercept using scale means

ggplot(df, aes(x=pfClaimScore, y = overclaimScore)) + 
geom_abline(intercept = data_intercept, slope=data_slope) +
theme_bw(base_size = 6) +
ggtitle("Atir - Original") + 
xlab("Perceived Financial Knowledge") + 
ylab("Overclaiming") +
scale_y_continuous(limits = c(0,0.8)) +
scale_x_continuous(limits = c(1,7),breaks=1:7)

ggsave("figures/ehermann-original.png",width = 1.5,height=1.5,units="in")

# Replication plot

data.lm.raw = lm(formula = overclaimScore ~ pfClaimScore + scale(accuracy), data = df)
data_intercept <- data.lm.raw$coefficients[1]
data_slope <- data.lm.raw$coefficients[2]
ggplot(df, aes(x=pfClaimScore, y = overclaimScore)) + 
  geom_point(alpha=.3) + 
  geom_abline(intercept = data_intercept, slope=data_slope) +
  theme_bw(base_size = 6) +
  ggtitle("Atir - Replication") + 
  xlab("Perceived Financial Knowledge") + 
  ylab("Overclaiming") +
  scale_y_continuous(limits = c(0,0.8)) +
  scale_x_continuous(limits = c(1,7),breaks=1:7)

ggsave("figures/ehermann-replication.png",width = 1.5,height=1.5,units="in")
