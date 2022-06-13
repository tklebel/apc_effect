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
# 9.7m works

# join and filter works
selected_works <- works %>%
  inner_join(works_from_journals, by = c("id" = "work_id")) %>%
  filter(publication_year >= 2000 & publication_year < 2022,
         type == "journal-article",
         publication_date > date_added_to_doaj)

# selected_works %>% head(300) %>% collect() <- inspection_set
# View(inspection_set)
# # why do we have those articles with oa_status == NA?

spark_write_parquet(selected_works,
                    "/user/tklebel/apc_paper/selected_works.parquet",
                    partition_by = "publication_year",
                    mode = "overwrite")

# the above code removes a couple of items which are not journal articles
# # Source:     spark<?> [?? x 2]
# # Ordered by: desc(n)
# type                      n
# <chr>                   <int>
# 1 journal-article     8134270
# 2 NA                    32044
# 3 component             31959
# 4 posted-content        27285
# 5 peer-review           20059
# 6 proceedings-article   13679
# 7 book-chapter           3752
# 8 journal                3403
# 9 journal-issue          3301
# 10 dissertation           388

spark_disconnect(sc)


