Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 27
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "select_papers")

# read files
csv_reader("/user/tklebel/openalex/works_host_venues.csv.bz2", "works_venues")
works_venues <- tbl(sc, "works_venues")

# time how long this takes
t1 <- lubridate::now()

csv_reader("/user/tklebel/openalex/works.csv.bz2", "works", memory = FALSE)
works <- tbl(sc, "works")

lubridate::now() - t1

csv_reader("/user/tklebel/openalex/venues_in_doaj.csv", "venues_in_doaj")
venues_in_doaj <- tbl(sc, "venues_in_doaj")
venue_ids <- venues_in_doaj %>% select(id, date_added_to_doaj)

works_from_journals <- works_venues %>%
  inner_join(venue_ids, by = c("venue_id" = "id"))

works_from_journals %>%
  count()
# 10.86m works

# join and filter works
selected_works <- works %>%
  inner_join(works_from_journals, by = c("id" = "work_id")) %>%
  filter(publication_year >= 2009 & publication_year < 2020,
         # only keep journal articles to remove some stray items like
         # peer-reviews
         type == "journal-article",
         publication_date > date_added_to_doaj)

check(selected_works)

# selected_works %>% head(300) %>% collect() <- inspection_set
# View(inspection_set)
# # why do we have those articles with oa_status == NA?

spark_write_parquet(selected_works,
                    "/user/tklebel/apc_paper/selected_works.parquet",
                    partition_by = "publication_year",
                    mode = "overwrite")

spark_disconnect(sc)
