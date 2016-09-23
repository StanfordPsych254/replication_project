library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

project_plan <- read_csv("data/planning_and_outcomes.csv")

# Read in project_info data from each project

# Initialize an error var
errors <- list()

# initialize an empty dataframe
project_data_summary <- data.frame( 
  project_key = character()
)

# Find user names that are included in 
project_keys <- project_plan$project_key 

passing <- c("salehi","smraposo","jedtan","mkeil","jreynolds","jmarias","jsalloum","ehermann",
              "lampinen","auc","rhiac")
failing <- c( NA )

# Run all respective R code - need loop because project_keys same variable name
for(project_key in passing) { # project_keys
  fp <- paste0("project_analyses/",project_key,".R")
  
  print(fp)
  tryCatch(source(fp), 
           error = function(e) {
             print(e)
             }
           ) 
  
  # If has project_info dataframe, add to all data
  if(exists("project_info")) { 
    project_data_summary <- bind_rows(project_data_summary,project_info) 
    rm("project_info") # In case other file does not same
  } else {
    errors <- c(errors,paste0(project_key," is missing project_info"))
  }
}

write_csv(project_data_summary, "data/project_data_summary.csv")
