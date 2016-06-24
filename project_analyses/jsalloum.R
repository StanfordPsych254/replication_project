library(tidyr)
library(dplyr)
library(ggplot2)
library(rjson)
library(tidyjson)

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

project_info <- data.frame(
  project_key = "jsalloum", 
  rep_final_n = length(d.af$speakerId), 
  rep_n_excluded = 0, 
  rep_es = F_stat, 
  rep_test_statistic = stat_descript,
  rep_p_value = p)
