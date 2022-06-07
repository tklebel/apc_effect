Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 6 # this can go up to 17, but in actuality, it seems to max out at 6
config$spark.executor.memory <- "60G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "OA_APCs")

spark_read_csv(sc, "/user/tklebel/openalex/works_host_venues.csv.bz2",
               name = "works_venues", memory = TRUE)
works_venues <- tbl(sc, "works_venues")

spark_read_csv(sc, "/user/tklebel/openalex/works.csv.bz2",
               name = "works", memory = FALSE)
works <- tbl(sc, "works")

spark_read_csv(sc, "/user/tklebel/openalex/venues_in_doaj.csv",
               name = "venues", memory = TRUE)
venues <- tbl(sc, "venues")
venue_ids <- venues %>% select(id)

works_from_journals <- works_venues %>%
  inner_join(venue_ids, by = c("venue_id" = "id"))

works_from_journals %>%
  count()
# 9.7m works

# join and filter works
works %>%
  inner_join(works_from_journals, by = c("id" = "work_id")) %>%
  filter(publication_year > 2000 | publication_year < 2022) %>%
  spark_write_parquet("/user/tklebel/apc_paper/selected_works.parquet",
                      partition_by = "publication_year")


selected_works <- spark_read_parquet(sc, name = "test",
                                     path = "/user/tklebel/apc_paper/selected_works.parquet")

spark_disconnect(sc)
