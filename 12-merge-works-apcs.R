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
                    app_name = "merge_works_apcs")


spark_read_parquet(sc, "/user/tklebel/apc_paper/fractional_works.parquet",
                   name = "fractional_works", memory = TRUE)
fractional_works <- tbl(sc, "fractional_works")

csv_reader("/user/tklebel/openalex/institutions_geo.csv.bz2", "institutions_geo")
institutions_geo <- tbl(sc, "institutions_geo")

csv_reader("/user/tklebel/openalex/venues_in_doaj.csv", "venues_in_doaj")
venues_in_doaj <- tbl(sc, "venues_in_doaj")

venues_apcs <- venues_in_doaj %>%
  select(id, APC, waiver, APC_in_dollar)

# the single remaining issue seems to be: some journals only become OA later
# so we should filter out those articles where the publication year is smaller
# than the year the journal got into DOAJ

fractional_works %>% count(is_oa)
# # Source: spark<?> [?? x 2]
#   is_oa       n
#   <lgl>   <int>
# 1 NA     211780
# 2 TRUE  4499346
# 3 FALSE    9405

# those that are not OA might be errors in OpenAlex data in terms of publication
# date.
# those that are NA might be those that are simply not covered by Unpaywall.
# we only keep those that are OA, which is done in the next script
# get journal for work -> get APC value and Ptop_10
joined <- fractional_works %>%
  left_join(venues_apcs, by = c("venue_id" = "id"))

# how many works remain?
joined %>%
  distinct(id) %>%
  count()
# # Source: spark<?> [?? x 1]
#           n
#       <int>
#   1 2499446

# add in geographi information from OpenAlex
out <- joined %>%
  left_join(institutions_geo, by = "institution_id")

spark_write_parquet(out, "/user/tklebel/apc_paper/all_papers_merged_wo_leiden.parquet",
                    partition_by = "publication_year",
                    mode = "overwrite")

spark_disconnect(sc)
