Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 12 # this can go up to 27
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


# get linking issn data
linking_issn <- read_tsv("data/external/20220613.ISSN-to-ISSN-L.txt")

# get a first base for matching (some journals might have either a print ISSN
# or an E-ISSN)
doaj <- doaj %>%
  mutate(linking_base = coalesce(pissn, eissn))

doaj_with_linking <- doaj %>%
  left_join(linking_issn, by = c("linking_base" = "ISSN"))


doaj_with_linking %>%
  select(journal_title, pissn, eissn, linking_base, `ISSN-L`) %>% View()

doaj_with_linking %>%
  summarise(missing_issnl = sum(is.na(`ISSN-L`)))
# 17 journals where the linking didnt work

doaj_with_linking %>%
  select(journal_title, pissn, eissn, linking_base, `ISSN-L`) %>%
  filter(is.na(`ISSN-L`)) %>%
  View()
# there the pissn seems not to be listed in the linking issn table, but the
# eissn might

unlinked <- doaj_with_linking %>%
  filter(is.na(`ISSN-L`))

linked <- anti_join(doaj_with_linking, unlinked)

re_linked <- unlinked %>%
  select(-`ISSN-L`) %>%
  mutate(linking_base = eissn) %>%
  left_join(linking_issn, by = c("linking_base" = "ISSN")) %>%
  filter(!is.na(`ISSN-L`))
# there is one single case where the linking failed:
# Revista de Sistemas, Cibernética e Informática
# 1690-8627
# This ISSN is not registered with the ISSN Centre


all_linked <- linked %>%
  bind_rows(re_linked)
nrow(all_linked)
# so 17717 journals from DAOJ (all but one) now have an ISSN-L

venues_joined <- venues_local %>%
  left_join(all_linked, by = c("issn_l" = "ISSN-L"))

venues_in_doaj <- venues_joined %>%
  filter(!is.na(journal_title))
nrow(venues_in_doaj)
# we matched 15640 out of 17717 journals that came from DOAJ with a valid ISSN
# that is 88.2768% of DOAJ journals were found in OpenAlex data


# openalex has quite a few as not being in DOAJ while we merged them
venues_in_doaj %>% count(is_in_doaj)
# # A tibble: 3 × 2
#   is_in_doaj     n
#   <lgl>      <int>
# 1 FALSE        731
# 2 TRUE       13799
# 3 NA          1110

# check them out wheter those are mistakes
openalex_errors <- venues_in_doaj %>%
  filter(!is_in_doaj | is.na(is_in_doaj)) %>%
  select(id, issn_l, issn, pissn, eissn, display_name, journal_title, publisher.x,
         publisher.y)
openalex_errors %>%
  View()
# this seems to be an error on the side of OpenAlex. Most have exact identical
# names. where this is not the case, sometimes it is just the difference of
# letters (latin vs cyrillic vs arabic, etc.), or sometimes different versions
# of the same name (but checked that they are identical, and in these cases
# both DOAJ and OpenAlex point e.g. to the same website)
# should report this to OpenAlex at some point
openalex_errors %>%
  write_csv("data/processed/openalex_doaj_errors.csv")

venues_in_doaj %>%
  write_csv("data/processed/venues_in_doaj.csv")

spark_disconnect(sc)
