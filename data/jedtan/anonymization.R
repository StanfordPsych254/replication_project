#MCF anonymization
rawDem$id <- paste0("anon",1:19)
rdd <- left_join(rawD, select(rawDem, WorkerID, id))
rdd$WorkerID <- rdd$id
rdd <- select(rdd, -id)
rawDem$WorkerID <- rawDem$id
rawDem <- select(rawDem, -id)

excluded <- rawD %>%
  filter(Trial <= 2, Time > 30000, Block > 0)

readr::write_csv(rdd, "data/jedtan/final_results_anon.csv")
readr::write_csv(rawDem, "data/jedtan/final_demographics_anon.csv")
