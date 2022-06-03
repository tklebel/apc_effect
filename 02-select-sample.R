Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
library(patchwork)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 10 # this can go up to 17, but in actuality, it seems to max out at 6
config$spark.executor.memory <- "60G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "OA_APCs")


spark_read_csv(sc, "/user/tklebel/openalex/venues.csv.bz2", escape = '\"',
               name = "venues", memory = TRUE)

venues <- tbl(sc, "venues")

venues_in_doaj <- venues %>%
  filter(is_in_doaj == TRUE, !is.na(issn_l))
venues_in_doaj <- venues_in_doaj %>% collect()
nrow(venues_in_doaj)
# so we have 14402 journals in DOAJ with an issn

doaj <- read_csv("data/external/journalcsv__doaj_20220603_1535_utf8.csv")
doaj

# manufacture the issn_l, let's see if it works
doaj_thin <- doaj %>%
  select(title = `Journal title`, pissn = `Journal ISSN (print version)`,
         eissn = `Journal EISSN (online version)`,
         APC, APC_amount = `APC amount`) %>%
  mutate(issn_l = coalesce(pissn, eissn))

venues_joined <- venues_in_doaj %>%
  left_join(doaj_thin, by = "issn_l")

# examine those that were not joined
venues_joined %>%
  filter(is.na(APC))
# quite a few. not sure why this happens
# also, maybe try to join the doaj data to the whole of openalex, since the
# flag there might be misleading.
# inquired here: https://www.issn.org/services/online-services/access-to-issn-l-table/


spark_disconnect(sc)
