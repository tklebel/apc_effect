Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 10 # this can go up to 27, depending on RAM
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "filter_works_and_cols")

spark_read_parquet(
  sc, "works_base", "/user/tklebel/apc_paper/all_papers_merged_wo_leiden.parquet",
  memory = FALSE
)
works <- tbl(sc, "works_base")

# only keep works that are OA according to unpaywall
works_oa <- works %>%
  filter(is_oa == TRUE)


selected_cols <- works_oa %>%
  select(id, doi, title, venue_id, author_position, institution_id, country,
         country_code, work_frac, APC, waiver, APC_in_dollar, publication_year)

# check(selected_cols, sampling = TRUE)

# fix type of APC column
selected_cols <- selected_cols %>%
  mutate(APC_in_dollar = as.numeric(APC_in_dollar))

selected_cols %>%
  spark_write_parquet("/user/tklebel/apc_paper/all_papers_selected_cols_wo_leiden.parquet",
                      mode = "overwrite")

spark_disconnect(sc)
