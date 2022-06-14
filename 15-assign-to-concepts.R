Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 27 # this can go up to 27, depending on RAM
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "assign_to_concepts")

spark_read_parquet(
  sc, "works", "/user/tklebel/apc_paper/all_papers_selected_cols.parquet",
  memory = FALSE
)
works <- tbl(sc, "works")

csv_reader("/user/tklebel/openalex/concepts.csv.bz2", "concepts",
           memory = FALSE)
concepts <- tbl(sc, "concepts")

concepts <- concepts %>%
  select(id, display_name, level) %>%
  filter(level == 0)
concepts

csv_reader("/user/tklebel/openalex/works_concepts.csv.bz2", "works_concepts")
works_concepts <- tbl(sc, "works_concepts")


# only deal with our works
works_with_concept_id <- works %>%
  distinct(id) %>% # need to take distinct work ids, otherwise we match in
  # duplicated fashion
  left_join(works_concepts, by = c("id" = "work_id"))
# sdf_register(works_with_concept_id, "works_with_concept_id")
# tbl_cache(sc, "works_with_concept_id")

work_ids_with_concepts <- works_with_concept_id %>%
  left_join(concepts, by = c("concept_id" = "id")) %>%
  filter(!is.na(level))

concept_fraction <- work_ids_with_concepts %>%
  group_by(id) %>%
  mutate(concept_frac = score/sum(score))

works <- works %>%
  left_join(concept_fraction, by = "id")

spark_write_parquet(works,
                    "/user/tklebel/apc_paper/papers_with_concepts.parquet",
                    mode = "overwrite")

spark_disconnect(sc)
