Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 27 # this can go up to 17, but in actuality, it seems to max out at 6
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "select_journals")


spark_read_csv(sc, "/user/tklebel/openalex/venues.csv.bz2", escape = '\"',
               name = "venues", memory = TRUE)

venues <- tbl(sc, "venues")

venues_local <- venues %>%
  filter(!is.na(issn_l)) %>%
  collect()
nrow(venues_local)
# there are 112215 venues with an issn as of now in openalex

doaj <- read_csv("data/processed/doaj_cleaned.csv")
doaj

# manufacture the issn_l, let's see if it works
doaj <- doaj %>%
  mutate(issn_l = coalesce(pissn, eissn))

venues_joined <- venues_local %>%
  left_join(doaj, by = "issn_l")

venues_in_doaj <- venues_joined %>%
  filter(!is.na(journal_title))

# openalex has quite a few as not being in DOAJ while we merged them
venues_in_doaj %>% count(is_in_doaj)

# check them out whetere those are mistakes
venues_in_doaj %>%
  filter(!is_in_doaj | is.na(is_in_doaj)) %>%
  select(id, issn_l, issn, pissn, eissn, display_name, journal_title, publisher.x,
         publisher.y)
# this seems to be an error on the side of OpenAlex. Most have exact identical
# names. where this is not the case, sometimes it is just the difference of
# letters (latin vs cyrillic vs arabic, etc.), or sometimes different versions
# of the same name (but checked that they are identical, and in these cases
# both DOAJ and OpenAlex point e.g. to the same website)
# should report this to OpenAlex at some point
#
# inquired here: https://www.issn.org/services/online-services/access-to-issn-l-table/
# this should improve the matching even further


venues_in_doaj %>%
  write_csv("data/processed/venues_in_doaj.csv")

spark_disconnect(sc)
