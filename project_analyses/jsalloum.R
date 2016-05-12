setwd("~/Repos/replication_project/")

loadLibraries = function() {
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(rjson)
  library(tidyjson)
  library(lme4)
  library(lmerTest)
  library(gridExtra)
  library(lsr)
}
suppressMessages(loadLibraries())

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
rs1.1 = aov(behaviorScore ~ plev * speakerSex, data = d.af)
summary(rs1.1)
