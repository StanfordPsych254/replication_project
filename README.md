# replication_project

These are the replications from Stanford Psych 254 - Winter 2016. This repository documents the analysis of all class projects. 

## Preprocessing

To preprocess, run `project_preprocessing.R`, which runs minimal analyses for all projects and caches a `project_data_summary` data frame.

All project-specific scripts live in `project_analyses`.

Scripts return a one-row dataframe with the following format:

`project_info <- data.frame(project_key = "XYZ",
                           rep_final_n = N,
                           rep_n_excluded = N,
                           rep_es = XYZ,
                           rep_test_statistic = XYZ,
                           rep_p_value = XYZ)`

## Data

Data files live in `processed_data`

## Packrat

Before running code, call `packrat: restore()` 