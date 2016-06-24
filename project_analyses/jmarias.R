library(tidyr)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(Hmisc)
library(xtable)
library(knitr)
library(rjson)
library(tidyjson)
library(memisc)

#Script for generating clean correlation table (bottom triangle only) with significance stars
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
} 

#Temporarily changing wd to see where the error files are located within the vector of files in the json folder - to comment out

#Reading the path
path <- "data/jmarias/"
files <- dir(path, 
             pattern = "*.json")
d <- data.frame()

#Transforming data from JSON format to dataframe
#?fromJSON

#reading the json files and turning them into one data.frame
for (f in files) {
  jf <- paste0(path,f)
  jd <- fromJSON(file = jf)                      #(paste(readLines(jf), collapse=""))
  id <- data.frame(workerid = jd$WorkerId,
                   accepttime = jd$AcceptTime,
                   submittime = jd$SubmitTime,
                   trial_number = jd$answers$data$trial_number,
                   block_number = jd$answers$data$block_number,
                   trial_type = jd$answers$data$trial_type,
                   stimulus = jd$answers$data$stimulus,
                   rating_raw = jd$answers$data$rating,
                   RT = as.numeric(jd$answers$data$rt),
                   age = jd$answers$data$age,  #age = as.numeric(jd$answers$data$age),
                   gender = jd$answers$data$gender,
                   education = jd$answers$data$education,  
                          #education = as.factor(jd$answers$data$education),
                   student = jd$answers$data$student,
                   homelang = jd$answers$data$homelang,
                   ethnicity = jd$answers$data$ethnicity,
                             #student = as.factor(jd$answers$data$student),
                             #homelang = as.character(jd$answers$data$homelang),
                             #ethnicity = as.factor(jd$answers$data$ethnicity),
                   race = jd$answers$data$race,
                   children = jd$answers$data$children,
                   mathhwhelp = jd$answers$data$mathhwhelp,
                   childAgeOld = jd$answers$data$childAgeOld,
                   expt_aim = jd$answers$data$expt_aim,
                   expt_gen = jd$answers$data$expt_gen)
 d <- bind_rows(d, id)
}



#warnings()
#warnings()  ##(Commented out for now) -- shows lots of NAs introduced by coercion
#Number of participants
length(unique(d$workerid))

d$gender <- as.factor(d$gender)
d$age <- as.numeric(d$age)
d$education <- as.factor(d$education)
d$student <- as.factor(d$student)
d$homelang <- as.character(d$homelang)
d$ethnicity <- as.factor(d$ethnicity)

#Setting stimulus variable as string
d$stimulus <- as.character(d$stimulus)
d$race <- as.factor(d$race)
d$children <- as.factor(d$children)
d$mathhwhelp <- as.factor(d$mathhwhelp)
d$childAgeOld <- as.factor(d$childAgeOld)
d$expt_gen <- as.character(d$expt_gen)

#generating a unique "accuracy" variable from stimulus for PVT practice and performance trials
#First as a string variable, including "dont know" and "not responded" as separate categories, then as a numeric binary, only including "accurate" as 1 and all else as 0. 

d1 <- d %>% 
    mutate(accuracy.string = ifelse(trial_type=="PVTpractice" | 
                             trial_type=="performance", 
                      rating_raw, NA)) %>% 
    mutate(accuracy = ifelse(accuracy.string=="accurate",
                            1, ifelse(accuracy.string=="inaccurate" |
                                        accuracy.string=="dontknow" |
                                        accuracy.string=="not responded",
                            0, NA)))

             

#Generating "pvt.acc" for accuracy on only the pvt performance trials (no practice)
d1 <- d1 %>% 
    mutate(pvt.acc = ifelse(trial_type=="performance",
                            accuracy, NA))

#Generating numeric ratings from the raw rating variable for the surveys
d2 <- d1 %>% 
    mutate(rating_2rev = ifelse(trial_type =="genAnx" |
                                trial_type == "mathAnx" |
                                trial_type == "mathMot", 
                      rating_raw, NA))
d2$rating_2rev <- as.numeric(d2$rating_2rev)

#Reverse scoring the 9 items from the general anxiety scale that need to be reversed
d3 <- d2 %>% 
      mutate(rating = ifelse(stimulus=='I feel pleasant.'|
                             stimulus=='I feel satisfied with myself.' |
                             stimulus=='I feel rested.' |
                             stimulus=='I am "calm, cool, and collected".' |
                             stimulus=='I am happy.' |
                             stimulus=='I feel secure.' |
                             stimulus=='I make decisions easily.' |
                             stimulus=='I am content.' |
                             stimulus=='I am a steady person.', 
                    (5 - rating_2rev), rating_2rev))
d <- d3

##########

### Math Motivation (3-item; see Chiu & Zeng, 2008) ######
d <- d %>% 
    mutate(mathmot = ifelse(trial_type =="mathMot",
                      rating, NA))


### Math Anxiety (30-item MARS-B; see Suinn & Winston, 2003) ###
  #No data prep needed aside from re-naming and calculating mean value for each participant, below
d <- d %>% 
    mutate(mathanx = ifelse(trial_type =="mathAnx",
                      rating, NA))

### General Anxiety (20-item (Trait Anxiety); see State-Trait Anxiety Inventory, Spielberger, 1983)
d <- d %>% 
    mutate(genanx = ifelse(trial_type =="genAnx",
                      rating, NA))


## Problem Verification Task (PVT) Performance  (see Rinne & Mazzocco, 2014) ####################


#The following analyses assume dataframe is called "d", reaction time of all PVT problems (in ms) is called "RT", accuracy (0 or 1) of each PVT problem is called "pvt.acc"

##Generating new RT variable to only include RTs for performance trials

d <- d %>% 
  mutate(perf.RT = ifelse(trial_type=="performance",
                            RT, NA))



#Change reaction time (in ms) to reaction time in SECONDS
#Following method in Wang et al. (2015; see pg. 1866) 
d <- d %>% 
  mutate(sec.RT = perf.RT/1000)



#### Mean Index and Standardized Scores for all measures #######
#str(d)
#Calculate mean 
subjectmeans <- d %>% 
  group_by(gender, age, workerid) %>% 
  summarise(mean.acc = mean(pvt.acc, na.rm=T), 
            mean.RT = mean(sec.RT, na.rm=T), 
            mean.mathmot = mean(mathmot, na.rm=T), 
            mean.mathanx = mean(mathanx, na.rm=T), 
            mean.genanx = mean(genanx, na.rm=T)) %>% 
  mutate(pvt = (mean.acc*100) / mean.RT)


 #Generate z-scores for variables (using "scale" function) 
subjectmeans$z.RT <- scale(subjectmeans$mean.RT, center = TRUE, scale = TRUE)
subjectmeans$z.pvt <- scale(subjectmeans$pvt, center = TRUE, scale = TRUE)
subjectmeans$z.mathmot <- scale(subjectmeans$mean.mathmot, center = TRUE, scale = TRUE)
subjectmeans$z.mathanx <- scale(subjectmeans$mean.mathanx, center = TRUE, scale = TRUE)
subjectmeans$z.genanx <- scale(subjectmeans$mean.genanx, center = TRUE, scale = TRUE)


##### Finding and Excluding Outliers - Confirm outliers

z.toofast <- subjectmeans %>% 
              filter(as.numeric(z.RT) <= -2.5)


subjectstokeep <- subjectmeans %>% 
                  filter(as.numeric(z.RT) > -2.5)

subjectmeans <- subjectstokeep

z.pvthigh <- subjectmeans %>% 
                filter(as.numeric(z.pvt) > 2)

# Save subjectmeans file
# Save intermediate file
path <- "processed_data/"
write.csv(subjectmeans, file = paste0(path,"jmarias.csv")) #Check excel file for any issues with data



#################################################################################### 
### Confirmatory
#################################################################################### 

mathperflm4 <- lm(z.pvt ~ age + gender + z.genanx + I(z.genanx^2) + z.mathanx * z.mathmot + (I(z.mathanx^2) * z.mathmot), data = subjectmeans)
pval <- summary(mathperflm4)$coef["z.mathmot:I(z.mathanx^2)","Pr(>|t|)"]
tval <- round(summary(mathperflm4)$coef["z.mathmot:I(z.mathanx^2)","t value"],3)
est <- round(summary(mathperflm4)$coef["z.mathmot:I(z.mathanx^2)","Estimate"],3)
df <- mathperflm4$df


stat_descript <- paste0("t(",df,") = ",tval)



project_info <- data.frame(
  project_key = "jmarias", 
  rep_final_n = nrow(subjectmeans), 
  rep_n_excluded = length(unique(d$workerid))-nrow(subjectmeans) , 
  rep_es = est, 
  rep_test_statistic = stat_descript,
  rep_p_value = pval,
  notes= "double check participant summaries and exclusion criteria"
)


