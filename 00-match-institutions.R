Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 10 # this can go up to 27
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "match_institutions")

spark_read_csv(sc, "/user/tklebel/openalex/institutions.csv.bz2", escape = '\"',
               name = "institutions", memory = TRUE)
institutions <- tbl(sc, "institutions")

spark_read_csv(sc, "/user/tklebel/openalex/institutions_geo.csv.bz2", escape = '\"',
               name = "institutions_geo", memory = TRUE)
institutions_geo <- tbl(sc, "institutions_geo") %>%
  select(institution_id, country)



institutions_selection <- institutions %>%
  select(id, display_name, display_name_alternatives, country_code) %>%
  left_join(institutions_geo, by = c("id" = "institution_id"))
institutions_selection

institutions_local <- institutions_selection %>%
  collect()

leiden_local <- read_csv("data/external/CWTS_Leiden_Ranking_2021.csv",
                         show_col_types = FALSE)

leiden_small <- leiden_local %>%
  filter(Field == "All sciences", Period == "2016–2019",
         Frac_counting == 0) %>%
  select(University, Country)

leiden_small_for_match <- leiden_small %>%
  mutate(university_normalized = str_to_lower(University) %>%
           stringi::stri_trans_general(id = "latin-ascii") %>%
           str_remove_all(",") %>%
           str_replace_all("\\&", "and"))

institutions_local <- institutions_local %>%
  mutate(university_normalized = str_to_lower(display_name) %>%
           stringi::stri_trans_general(id = "latin-ascii") %>%
           str_remove_all(",") %>%
           str_replace_all("\\&", "and"))


joined <- leiden_small_for_match %>%
  left_join(institutions_local)

joined %>%
  filter(is.na(display_name))
# 179 unmatched

# need to glance over the matched ones as well
joined %>%
  filter(!is.na(display_name)) %>%
  summarise(country_match = all(Country == country))
# nope, check out those.

country_mismatch <- joined %>%
  filter(Country != country)

View(country_mismatch)

# and also multiple matches
multiple_matches <- joined %>%
  count(University) %>%
  filter(n > 1) %>%
  left_join(joined)

View(multiple_matches)

# only work with those that worked for now
success <- joined %>%
  filter(Country == country)

success %>%
  count(University) %>%
  arrange(desc(n))

# for the remaining duplicates:
# - https://openalex.org/I3124059619 is the correct version for china university
# of geosciences (checked with the map on leiden ranking website)
#  therefore removing https://openalex.org/I3125743391
#
# - University of rennes 1 both point to the same university in openalex, so
# we should keep both (https://openalex.org/I3123023596 and https://openalex.org/I56067802)
#
# for medical university of lodz, also both should be kept: https://openalex.org/I4210122071
# https://openalex.org/I866987647
#
# continue for now building the pipeline, fix matching issues later
# issues:
# - remaining unmatched ~180 instutions
# - checking whether the matched institutions are correct (via eye)

# add the two in, and remove the wrong chinese one
keepers <- joined %>%
  filter(id %in% c("https://openalex.org/I866987647",
                   "https://openalex.org/I3123023596"))

intermediate_success <- success %>%
  filter(id != "https://openalex.org/I3125743391") %>%
  bind_rows(keepers)

intermediate_success %>%
  write_csv("data/processed/leiden_matched.csv")

spark_disconnect(sc)
