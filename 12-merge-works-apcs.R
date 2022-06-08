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


# get journal for work -> get APC value and PPtop_10

spark_disconnect(sc)
