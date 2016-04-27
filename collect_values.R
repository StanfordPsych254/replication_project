library(tidyr)
library(dplyr)
library(rjson)
library(ggplot2)
library(lme4)
library(langcog) # devtools::install_github("langcog/langcog")
sem <- function(x) {sd(x, na.rm=TRUE) / sqrt(length(x))}
ci95 <- function(x) {sem(x) * 1.96}
