Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 15 # this can go up to 27, depending on RAM
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "filter_works_and_cols")

spark_read_parquet(
  sc, "works_base", "/user/tklebel/apc_paper/all_papers_merged.parquet",
  memory = FALSE
)
works <- tbl(sc, "works_base")

# only keep works that are OA according to unpaywall
works_oa <- works %>%
  filter(is_oa == TRUE)

# only keep works that are from universities which are matched to Leiden
works_w_leiden <- works_oa %>%
  filter(!is.na(Period))

check(works_w_leiden)

# only keep rows where publication year is the last year of the leiden period
matched_works <- works_w_leiden %>%
  mutate(first_year_of_period = regexp_extract(Period, "^(\\\\d{4})", 1) %>% as.numeric(),
         last_year_of_period = regexp_extract(Period, "(\\\\d{4})$", 1) %>% as.numeric()) %>%
  filter(publication_year <= last_year_of_period &
           publication_year >= first_year_of_period)

check(matched_works)

selected_cols <- matched_works %>%
  select(id, doi, title, venue_id, author_position, institution_id, work_frac,
         APC, waiver, APC_in_dollar, University, country = Country1,
         country_code, Period, P_top10, publication_year, first_year_of_period,
         last_year_of_period)

check(selected_cols, sampling = TRUE)

# fix type of APC column
selected_cols <- selected_cols %>%
  mutate(APC_in_dollar = as.numeric(APC_in_dollar))

selected_cols %>%
  spark_write_parquet("/user/tklebel/apc_paper/all_papers_selected_cols.parquet",
                      mode = "overwrite")

spark_disconnect(sc)
