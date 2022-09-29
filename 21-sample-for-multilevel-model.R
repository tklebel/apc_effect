Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
library(patchwork)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 8 # this can go up to 27, depending on RAM
config$spark.executor.memory <- "25G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "OA_APCs")
message("Connection to Spark successful!")

spark_read_parquet(
  sc, "works", "/user/tklebel/apc_paper/papers_with_concepts.parquet",
  memory = TRUE
)
works <- tbl(sc, "works")

# only do for 2016-2019 for now
# better would be to use a single value for pptop10, but here we have it as a
# varying one.

selected_works <- works %>%
  filter(!is.na(field),
         # select period 2016-2019, which includes the respective publications
         first_year_of_period == 2016)

# check that selection works
selected_works %>%
  summarise(min_year = min(publication_year),
            max_year = max(publication_year))

# we have overall low fractional authorships, and duplicate entries since
# authors have mutliple affiliations

# sample individual papers (so the probability of inclusion is similar
# regardless of how many authors or fields the paper belongs to)
only_papers <- selected_works %>%
  distinct(id)

n <- sdf_nrow(only_papers)
# 960469 papers are in this set

# sample 8% of papers -> this will lead to much more rows, since we have multiple
# institutions and fields
frac <- .08

the_sample <- only_papers %>%
  sdf_sample(fraction = frac, replacement = FALSE, seed = 20220929) %>%
  left_join(selected_works) %>%
  collect()

the_sample <- the_sample %>%
  # calculate weights for each observation
  mutate(total_weight = work_frac * concept_frac)


# checking the sample -----
# did we actually get about 80k papers?
the_sample %>%
  distinct(id) %>%
  nrow()
# yep: 76447

# what about the distribution of fields
the_sample %>%
  distinct(id, field, concept_frac) %>%
  group_by(field) %>%
  summarise(n = sum(concept_frac)) %>%
  arrange(desc(n))
# # A tibble: 19 × 2
#   field                      n
#    <chr>                  <dbl>
#  1 Medicine              23422.
#  2 Biology               12584.
#  3 Chemistry              7456.
#  4 Computer science       7185.
#  5 Materials science      6358.
#  6 Psychology             4483.
#  7 Physics                2458.
#  8 Environmental science  2257.
#  9 Political science      1875.
# 10 Geography              1602.
# 11 Sociology              1497.
# 12 Art                    1213.
# 13 Business               1158.
# 14 Mathematics             911.
# 15 Geology                 761.
# 16 Philosophy              527.
# 17 Economics               322.
# 18 History                 198.
# 19 Engineering             180.

# this conforms in general to the overall pattern, but the ordinal ranking is
# not identical (expected given this is a sample and some differences are small)


# full counting on authorships
the_sample %>%
  distinct(id, country) %>%
  count(country, sort = TRUE)
# # A tibble: 69 × 2
#    country            n
#    <chr>          <int>
#  1 China          15407
#  2 United States  13265
#  3 Brazil          6853
#  4 United Kingdom  4339
#  5 Germany         3461
#  6 Spain           2985
#  7 Japan           2790
#  8 South Korea     2723
#  9 Canada          2574
# 10 Australia       2544
# # … with 59 more rows


the_sample %>%
  write_csv("data/processed/multilevel_sample_large.csv")

spark_disconnect(sc)
