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
# choose 80k papers -> this will lead to much more rows, since we have multiple
# institutions and fields
frac <- 80000 / n

the_sample <- only_papers %>%
  sdf_sample(fraction = frac, replacement = FALSE, seed = 20220813) %>%
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
# yep: 80222

# what about the distribution of fields
the_sample %>%
  distinct(id, field, concept_frac) %>%
  group_by(field) %>%
  summarise(n = sum(concept_frac)) %>%
  arrange(desc(n))
# # A tibble: 19 × 2
#   field                      n
#   <chr>                  <dbl>
#  1 Medicine              17139.
#  2 Biology                7226.
#  3 Computer science       4225.
#  4 Materials science      4011.
#  5 Chemistry              3930.
#  6 Psychology             2390.
#  7 Physics                1654.
#  8 Environmental science  1182.
#  9 Political science       857.
# 10 Geography               761.
# 11 Sociology               636.
# 12 Art                     605.
# 13 Mathematics             554.
# 14 Business                527.
# 15 Geology                 428.
# 16 Philosophy              251.
# 17 Economics               121.
# 18 History                  66.9
# 19 Engineering              64.8

# this conforms in general to the overall pattern, but the ordinal ranking is
# not identical (expected given this is a sample and some differences are small)


# full counting on authorships
the_sample %>%
  distinct(id, country) %>%
  count(country, sort = TRUE)
# # A tibble: 69 × 2
#    country            n
#    <chr>          <int>
#  1 China          16248
#  2 United States  13959
#  3 Brazil          7075
#  4 United Kingdom  4602
#  5 Germany         3634
#  6 Spain           2982
#  7 South Korea     2878
#  8 Canada          2833
#  9 Japan           2820
# 10 Australia       2762
# # … with 59 more rows


the_sample %>%
  write_csv("data/processed/multilevel_sample_large.csv")

spark_disconnect(sc)
