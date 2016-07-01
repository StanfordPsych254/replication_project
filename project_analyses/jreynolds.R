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

stat_descript <- paste0("t(",df,") = ",round(tval, 3))

project_info <- data.frame(
  project_key = "jreynolds", 
  rep_t_stat = tval,
  rep_t_df = df,
  rep_final_n = length(unique(d$Participant)), 
  rep_n_excluded = 0, 
  rep_es = NA, 
  rep_test_statistic_str = stat_descript,
  rep_p_value = pval)