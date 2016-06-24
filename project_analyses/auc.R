library(readr)
library(jsonlite)
library(dplyr)

## Code in this block taken from: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- plyr::ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, mean = measurevar)
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
    # Measure var on left, idvar + between vars on right of formula.
    data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
     .fun = function(xx, col, na.rm) {
        c(subjMean = mean(xx[,col], na.rm=na.rm))
      },
      measurevar,
      na.rm
    )

    # Put the subject means with original data
    data <- merge(data, data.subjMean)

    # Get the normalized data in a new column
    measureNormedVar <- paste(measurevar, "_norm", sep="")
    data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
                               mean(data[,measurevar], na.rm=na.rm)

    # Remove this subject mean column
    data$subjMean <- NULL
    
    return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
    FUN=is.factor, FUN.VALUE=logical(1))

  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }

  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL

  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")

  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                           FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor

  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## Using the effect size $f^2 = 0.15$ with $\alpha = 0.05$ (original power analysis not reported), we did a post-hoc power analysis on the original sample size of 24 participants. To get 80% power, we need 43 participants in each condition, while 90% requires 55 participants and 95% power requires 70% participants. Our target for replication is the "multiple regression analysis in which we predicted the judgments using DFT, DFT-squared, judgment type (trustworthiness = 1, attractiveness = 0), and their interactions (all predictors centered), $F(5, 16) = 285.81, p < .001, R^2 = .99$".

## The planned sample would be US-based mTurkers, with 45 participants in each condition. We will not be able to replicate the exclusion criteria and instead will restrict the locale to US participants of all genders and all ages. We will collect light demographic information to analyze this difference.

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
                   exp_type = answers$type,
                   elapsed_ms = answers$elapsed_ms,
                   num_errors = answers$num_errors,
                   gender = answers$gender,
                   education = answers$education)
  d.raw <- bind_rows(d.raw, id)
}

d <- summarySEwithin(d.raw, measurevar="face_rating", withinvars=c("face_dft", "exp_type"),
                            idvar="workerid", na.rm=FALSE, conf.interval=.95)


mean_dft <- mean(as.numeric(as.character(d$face_dft)))
mean_face_rating <- mean(d$face_rating_norm)
d <- d %>%
  mutate(centered_face_rating = face_rating_norm - mean_face_rating,
         centered_dft = as.numeric(as.character(face_dft)) - mean_dft)

write_csv(d, "processed_data/auc.csv")

model <- lm(centered_face_rating ~
              exp_type + centered_dft + I(centered_dft ** 2) +
              centered_dft * exp_type + 
              I(centered_dft**2) * exp_type,
            d)
summary(model)

project_info <- data.frame(project_key = "auc",
                           n_final = length(unique(d.raw$workerid)),
                           n_excluded = 0)
                           

#This results in $F(5, 16) = 91.11, p < 0.001, R^2 = 0.97$ which is the same
#significance level and effect size as the original study. Comparing the
#coeffecient estimates from this model with that of the original study, the
#terms have the same sign though in absolute terms, ours are all less than 0.1.
#The original study found significant effects for all the coefficients, though
#we did not find significance for the judgement type and judgement type and
#DFT-squared.

# Finally, the authors computed a by-participant ANOVA with DFT as a repeated measure and experiment type as a between-subjects factor.


# Within-subjects means
ms <- d.raw %>%
  group_by(exp_type, face_dft, workerid) %>%
  dplyr::summarise(mean_rating = mean(face_rating))

ms$workerid <- as.factor(ms$workerid)
ms$face_dft <- as.factor(ms$face_dft)
ms$exp_type <- as.factor(ms$exp_type)

summary(aov(mean_rating ~ face_dft * exp_type + Error(workerid / face_dft), ms))

ms <- d.raw %>%
  group_by(exp_type, face_dft) %>%
  dplyr::summarise(mean_rating = mean(face_rating))

ms$face_dft <- as.factor(ms$face_dft)
ms$exp_type <- as.factor(ms$exp_type) 

summary(aov(mean_rating ~ face_dft + exp_type, ms))
