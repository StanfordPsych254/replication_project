library(rjson)
library(dplyr)
library(readr)
library(tidyr)
library(ggrepel)
library(lme4)

##### DATA --------------------------------------------------

## ORIGINAL

sofer <- read_csv("subsidiary_analysis/Experiment_1_Rps.csv")
sofer <- sofer %>%
  mutate(subid = 1:n()) %>%
  gather(datapoint, rating, starts_with("dft")) %>%
  separate(datapoint, into = c("foo","DFT","trial_num"), sep = "_") %>%
  mutate(condition = ifelse(Trust1Attrc2 == 1, "Trustworthiness", "Attractiveness"), 
         face_dft = as.numeric(DFT)) %>%
  select(-foo, -Trust1Attrc2, -DFT) %>% 
  group_by(subid, condition, face_dft) %>%
  summarize(rating = mean(rating))

## REPLICATION

path <- "data/auc/"
files <- dir(path, pattern = "*.json")
d.raw <- data.frame()
d.balance <- data.frame()

for (f in files) {
  jf <- paste0(path, f)
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  answers <- jd$answers$data
  race <- paste(answers$race, collapse="+")
  id <- data.frame(workerid = jd$WorkerId,
                   race = race,
                   age = answers$age,
                   face_dft = as.numeric(answers$face),
                   face_rating = as.numeric(answers$rating),
                   condition = answers$type,
                   elapsed_ms = answers$elapsed_ms,
                   num_errors = answers$num_errors,
                   gender = answers$gender,
                   education = answers$education)
  d.raw <- bind_rows(d.raw, id)
}

rep <- d.raw %>%
  group_by(workerid, face_dft, condition) %>%
  summarise(rating = mean(face_rating))

write_csv(rep, "processed_data/auc.csv")

##### PLOTS --------------------------------------------------

se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

## REPLICATION
d_plot_rep <- rep %>% 
  group_by(face_dft, condition, workerid) %>%
  summarise(rating = mean(rating)) %>%
  summarise(se = se(rating), 
            rating = mean(rating))

ggplot(d_plot_rep, aes(x=face_dft, y = rating, col = condition, group=condition)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) + 
  geom_point(position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin=rating-se, ymax=rating+se),
                size =.3,
                width =.3, 
                position = position_dodge(width = .5))  + 
  geom_label_repel(data = filter(d_plot_rep, face_dft==50), 
                   aes(label = condition), size = 2) + 
  theme_bw(base_size = 6) +
  scale_colour_grey(start=.4, guide=FALSE) + 
  ggtitle("Sofer - Replication") + 
  xlab("Distance from Typical") + 
  scale_y_continuous(name = "Rating", 
                     limits = c(1, 9), 
                     breaks = c(1, 3, 5, 7, 9))

ggsave("figures/auc-replication.png", width = 1.5, height=1.5,units="in")

### ORIGINAL
d_plot_orig <- sofer %>% 
  group_by(face_dft, condition, subid) %>%
  summarise(face_rating = mean(rating)) %>%
  summarise(se = se(face_rating), 
            face_rating = mean(face_rating)) %>%
  mutate(condition = factor(condition, 
                            levels = c("Attractiveness","Trustworthiness"), 
                            labels = c("attractive","trustworthy")))

ggplot(d_plot_orig, aes(x=face_dft, y = face_rating, col = condition, group=condition)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) + 
  geom_point(position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin=face_rating-se, ymax=face_rating+se),
                size =.3,
                width =.3, 
                position = position_dodge(width = .5))  + 
  geom_label_repel(data = filter(d_plot_orig, face_dft==50), 
                   aes(label = condition), size = 2) + 
  theme_bw(base_size = 6) +
  scale_colour_grey(start=.4, guide=FALSE) + 
  ggtitle("Sofer - Original") + 
  xlab("Distance from Typical") + 
  scale_y_continuous(name = "Rating", 
                     limits = c(1,9), 
                     breaks = c(1,3, 5, 7, 9))

ggsave("figures/auc-original.png",width = 1.5, height=1.5, units="in")


##### STATS --------------------------------------------------
# we select the more standard "by subjects" analysis for the final.
library(car)
idata = data.frame(dft = c("dft_0", "dft_10", "dft_20", "dft_30", "dft_40", "dft_50", 
                           "dft_60", "dft_70", "dft_80", "dft_90", "dft_100"))

## ORIGINAL 
shortForm.orig = sofer %>%
  ungroup %>%
  mutate(dft = factor(face_dft), 
         condition = factor(condition), 
         subid = factor(subid)) %>%
  select(dft, condition, subid, rating) %>%
  group_by(subid) %>%
  spread(dft, rating, sep = "_")

manova.orig = lm(cbind(dft_0, dft_10, dft_20, dft_30, dft_40, dft_50,
                      dft_60, dft_70, dft_80, dft_90, dft_100) ~ condition, 
                data = shortForm.orig)
manova.res.orig = Anova(manova.orig, idata= idata, idesign = ~dft)
manova.res.orig

## REPLICATION
shortForm.rep = rep %>%
  ungroup %>%
  mutate(dft = factor(face_dft), 
         condition = factor(condition), 
         workerid = factor(workerid)) %>%
  select(dft, condition, workerid, rating) %>%
  group_by(workerid) %>%
  spread(dft, rating, sep = "_")

manova.rep = lm(cbind(dft_0, dft_10, dft_20, dft_30, dft_40, dft_50,
                      dft_60, dft_70, dft_80, dft_90, dft_100) ~ condition, 
                data = shortForm.rep)
manova.res.rep = Anova(manova.rep, idata= idata, idesign = ~dft)
manova.res.rep 

# anovaObj = unlist(summary(rep.mod)[[2]])
# model_Fval = anovaObj["F value2"]
# model_dfnum = anovaObj["Df2"]
# model_dfdenom = anovaObj["Df3"]
# model_pval = anovaObj["Pr(>F)2"]

# It's too hard to extract vals from manova output, so we just manually pull from table

model_Fval = 3.2
model_dfnum = 10
model_dfdenom = 84

source("project_analyses/computeES.R")
es <- esComp(model_Fval, df1 = model_dfnum, df2 = model_dfdenom, esType = "F")

stat_descript <- paste0("F(",model_dfnum,",", model_dfdenom,") = ",round(model_Fval,2))

#### TO DO
project_info <- data.frame(project_key = "auc",
                           rep_final_n = length(unique(d.raw$workerid)),
                           rep_n_excluded = 0,
                           rep_es = es,
                           rep_test_statistic_str = stat_descript,
                           rep_t_stat = NA,
                           rep_t_df = NA,
                           rep_p_value = pval1,
                           notes="based on writeup and article, assuming conditiontrustworthy:centered_dft interaction is key stat of interest. Should clarify with Carolyn. Also make sure doing correct analysis from article (i.e. MANOVA)."
)

