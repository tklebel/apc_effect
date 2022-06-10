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
                    app_name = "OA_APCs")


spark_read_parquet(sc, "/user/tklebel/apc_paper/fractional_works.parquet",
                   name = "fractional_works", memory = TRUE)
fractional_works <- tbl(sc, "fractional_works")

fractional_works

csv_reader("/user/tklebel/openalex/venues_in_doaj.csv", "venues_in_doaj")
venues_in_doaj <- tbl(sc, "venues_in_doaj")

venues_apcs <- venues_in_doaj %>%
  select(id, APC, waiver, APC_in_dollar)


csv_reader("/user/tklebel/apc_paper/leiden_matched.csv", "leiden_key")
leiden_key <- tbl(sc, "leiden_key")

spark_read_csv(sc, "leiden", "/user/tklebel/apc_paper/leiden_2021.csv",
               memory = TRUE, null_value = "NA")
leiden <- tbl(sc, "leiden") %>%
  filter(Field == "All sciences", Frac_counting == 1,
         !is.na(PP_top10)) %>%
  select(University, Field, Period, Frac_counting, PP_top10)

# hm, this has issues. first, we included NA author positions
# second, there are works which are not OA, like https://explore.openalex.org/works/W2258570746
# This comes because some DOAJ journals were not OA before some date X.
# in the above example, the paper is from 2006, but the journal turned OA in 2013
# however, OpenAlex has the publication year at 2020.
# furthermore, in the "works_authorships" there is actually a first middle and
# last author designation, but it did not get transferred into our above data
# # ah, the join was the wrong way around

# the single remaining issue seems to be: some journals only become OA later
# so we should filter out those articles where the publication year is smaller
# than the year the journal got into DOAJ

fractional_works %>% count(is_oa)
# # Source: spark<?> [?? x 2]
#   is_oa       n
#   <lgl>   <int>
# 1 FALSE   41532
# 2 TRUE  6694172
# 3 NA     369903

# those that are not OA might be errors in OpenAlex data in terms of publication
# date.
# those that are NA might be those that are simply not covered by Unpaywall.


# get journal for work -> get APC value and PPtop_10
joined <- fractional_works %>%
  left_join(venues_apcs, by = c("venue_id" = "id")) %>%
  left_join(leiden_key, by = c("institution_id" = "id")) %>%
  left_join(leiden, by = c("University"))

# need to filter so the publication year fits to the leiden data year (check
# earlier code on this)


spark_write_parquet(joined, "/user/tklebel/apc_paper/all_papers_merged.parquet",
                    partition_by = "publication_year",
                    mode = "overwrite")



spark_disconnect(sc)
