Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 3 # this can go up to 17, but in actuality, it seems to max out at 6
config$spark.executor.memory <- "60G"
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

leiden_local <- read_csv("data/external/CWTS Leiden Ranking 2021.csv",
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
