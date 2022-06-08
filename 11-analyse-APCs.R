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


spark_read_parquet(sc, "/user/tklebel/apc_paper/selected_works.parquet",
                   name = "works", memory = TRUE)
works <- tbl(sc, "works")

csv_reader("/user/tklebel/openalex/works_authorships.csv.bz2",
           name = "works_authorships")
works_authorships <- tbl(sc, "works_authorships")

# assign works to institutions
# do fractional counting
# get journal for work -> get APC value and PPtop_10
