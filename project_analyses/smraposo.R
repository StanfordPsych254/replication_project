library(rjson)
library(dplyr)
library(tidyr)
library(stringr)

#Getting files
path <- "data/smraposo/"
files <- dir(path, 
             pattern = "*.json")
d.raw <- data.frame()

for (f in files) {
  jf <- paste0(path, f)
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  id <- data.frame(workerid = jd$WorkerId, 
                   ans = jd$answers)
  d.raw <- bind_rows(d.raw, id)
}

# Cleaning up data

#removing "ans.data." from all variable names
names(d.raw) <- gsub("ans.data.", "", names(d.raw))

# format data
d0 <- d.raw %>% 
  select(c(workerid, condition, timeMetric, incrAction, rt.2, rt.3),
         contains("responses")) %>% 
  rename(delay.rt = rt.2,
         savings.rt = rt.3,
         delaytime = responses,
         savings = responses.1,
         age = responses.2,
         demos = responses.3,
         comments = responses.4) %>%
  mutate(workerid = as.factor(workerid),
         condition = as.factor(condition),
         timeMetric = as.factor(timeMetric),
         incrAction = as.factor(incrAction))

#cleaning json responses
d0$delaytime <- gsub("Q0", "", d0$delaytime) #removing Q0 from responses
d0$delaytime = gsub("([.])|[[:punct:]]", "\\1", d0$delaytime) #removing all punctuation except .
d0$savings <- gsub("Q0", "", d0$savings) #removing Q0 from responses
d0$savings = gsub("([.])|[[:punct:]]", "\\1", d0$savings) #removing all punctuation except "."
d0$age <- gsub("Q0", "", d0$age) #removing Q0 from responses
d0$age = gsub("([.])|[[:punct:]]", "\\1", d0$age) #removing all punctuation except "."
d0$demos <- gsub("Q0", "", d0$demos) #removing Q0 from responses
d0$demos = gsub("([.$,-])|[[:punct:]]", "\\1", d0$demos) #removing all punctuation except "."

#adding separators in between demographic responses
d0$demos <- gsub(",Q1", "_", d0$demos) 
d0$demos <- gsub(",Q2", "_", d0$demos)
d0$demos <- gsub(",Q3", "_", d0$demos)
d0$comments <- gsub("Q0", "", d0$comments) 
d0$comments <- gsub("Q1", "_", d0$comments) 
d.demos <- d0 %>%
  separate(demos, sep="_", into=c("gender","edu","race","income")) %>%
  separate(comments, sep="_",into=c("purpose","comments")) %>%
  mutate(gendercat = as.factor(gender),
         educat = as.factor(edu),
         racecat = as.factor(race),
         incomecat = as.factor(income))

# Cleaning up factor labels 
d.demos$gendercat = factor(d.demos$gender, labels = c("Female", "Male", "Other", "Decline to state"), levels = c("Female", "Male", "Other", "Decline to state"))
d.demos$educat = factor(d.demos$educat, labels = c("high school diploma or less", "some college or technical school", "college degree", "some post-graduate", "post-graduate degree e.g., Masters, Ph.D., J.D.", "Decline to state"), levels = c("high school diploma or less", "some college or technical school", "college degree", "some post-graduate", "post-graduate degree e.g., Masters, Ph.D., J.D.", "Decline to state"))
d.demos$racecat = factor(d.demos$race, labels = c("White non-Hispanic", "White Hispanic", "Black or African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Decline to state"), levels = c("White non-Hispanic", "White Hispanic", "Black or African American", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Pacific Islander", "Decline to state"))
d.demos$incomecat = factor(d.demos$income, labels = c("<$20,000", "$20-$29,999", "$30-$39,999", "$40-$49,999", "$50-$59,999", "$60-$69,999", "$70-$79,999", "$80-$89,999", "$90-$99,999", "$100,000", "Decline to state"), levels = c("<$20,000", "$20-$29,999", "$30-$39,999", "$40-$49,999", "$50-$59,999", "$60-$69,999", "$70-$79,999", "$80-$89,999", "$90-$99,999", "$100,000", "Decline to state"))

d.demos$age <- as.numeric(d.demos$age)

d.demos <- d.demos %>% 
  mutate(gender = as.numeric(gendercat),
         edu = as.numeric(educat),
         race = as.numeric(racecat),
         income = as.numeric(incomecat))


#Computing delay time in years
d.demos$delaytime <- as.numeric(d.demos$delaytime)
d.demos$delaydiv <- ifelse(d.demos$timeMetric == "fine", 365, 1) #Creating a denominator, depending on condition
d.demos$delaytimeyrs = d.demos$delaytime / d.demos$delaydiv #Calculating time to wait in years

d = d.demos

# Write tidy file
path <- "processed_data/"
write.csv(d.demos, file = paste0(path,"smraposo.csv")) #Check excel file for any issues with data


#Exclusion criteria
#Creating binary variable for anyone who was >3SD from the mean of the main DV (when to begin saving)
d$delayexclude <- ifelse(d$delaytimeyrs > (mean(d$delaytimeyrs, na.rm = T) + 3*sd(d$delaytimeyrs, na.rm = T)), "exclude",
                          ifelse(d$delaytimeyrs == "NA", "exclude", "include")) 
tdelayexcl = table(d$delayexclude)

#Creating binary variable for anyone who would wait more than 10950 days in fine condition or 30 years in gross-grained condition
d$delayrawexclude1 <- ifelse((d$timeMetric == "fine" & d$delaytime > 10950), "exclude", "include") 
tdelayexclraw1 = table(d$delayrawexclude1)
d$delayrawexclude2 <- ifelse((d$timeMetric == "gross" & d$delaytime > 30), "exclude", "include") 
tdelayexclraw2 = table(d$delayrawexclude2)

#Overall exclusion variable based on 3 variables above (if anyone was excluded based on any of the 3 rules above)
d$exclude <- ifelse((d$delayexclude == "exclude" | d$delayrawexclude1 == "exclude" | d$delayrawexclude2 == "exclude"), "exclude", "include") 
tableexcl = table(d$exclude)

# Key Analysis
rs.lmdelaytot.cov = lm(delaytimeyrs ~ age + income + edu + timeMetric * incrAction, d[d$exclude=="include",])
delaycovres = anova(rs.lmdelaytot.cov)
Fstat1 = delaycovres[4,4] #time metric
dfnum1 = delaycovres[4,1]
dfdenom1 = rs.lmdelaytot.cov$df.residual
pval1 = delaycovres[4,5]

n_init <- dim(d)
n_final <- dim(d[d$exclude=="include",])
key_stat <- Fstat1
stat_descript <- paste0("F(",dfnum1,", ",dfdenom1,") = ",round(Fstat1, 3))

project_info <- data.frame(
  project_key = "smraposo", 
  rep_final_n = nrow(d[d$exclude=="include",]), 
  rep_n_excluded = nrow(d[d$exclude!="include",]), 
  rep_es = Fstat1, 
  rep_test_statistic = stat_descript,
  rep_p_value = pval1)
