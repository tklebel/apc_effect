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
                    app_name = "fractionalise_authorships")


spark_read_parquet(sc, "/user/tklebel/apc_paper/selected_works.parquet",
                   name = "works", memory = TRUE)
works <- tbl(sc, "works")

csv_reader("/user/tklebel/openalex/works_authorships.csv.bz2",
           name = "works_authorships")
works_authorships <- tbl(sc, "works_authorships")


# general exploration ----
works %>%
  count(is_retracted)

works %>%
  count(type) %>%
  arrange(desc(n))
# only journal articles, as it should be

works %>%
  summarise(min_date = min(publication_year),
            max_date = max(publication_year))
# this is as it should be, minimum is 2002. DOAJ was launched in 2003.


# assign works to institutions and do fractional counting -----
author_counts <- works_authorships %>%
  distinct(work_id, author_id) %>%
  count(work_id) %>%
  mutate(author_frac = 1/n)

fractional_authorships <- works_authorships %>%
  left_join(author_counts) %>%
  group_by(work_id) %>%
  # find multiple affiliations
  add_count(work_id, author_id, name = "n_institutions") %>%
  mutate(work_frac = author_frac / n_institutions)

# three works that can be used to validate the approach:
# c("https://openalex.org/W2056304178", "https://openalex.org/W2048244903",
#   "https://openalex.org/W2045612926")

fractional_authorships <- fractional_authorships %>%
  filter(author_position != "middle",
         !is.na(institution_id)) %>%
  select(work_id, author_position, institution_id, work_frac)

# # check out this single paper, to identify what goes on here
# fractional_authorships %>%
#   filter(work_id ==  "https://openalex.org/W2258570746")
#
# works %>%
#   filter(id ==  "https://openalex.org/W2258570746")
#
# # they simply have no institution_id, so all is fine

joined_works <- works %>%
  select(id, doi, title, publication_year, is_oa, venue_id) %>%
  inner_join(fractional_authorships, by = c("id" = "work_id"))
# was not sure about this inner join here, but it is correct:
# retains all authors for those papers where authors have an institution_id


joined_works %>%
  spark_write_parquet("/user/tklebel/apc_paper/fractional_works.parquet",
                      partition_by = "publication_year",
                      mode = "overwrite")

spark_disconnect(sc)
