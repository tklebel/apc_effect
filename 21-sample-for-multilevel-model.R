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
  filter(!is.na(display_name),
         publication_year > 2015 & publication_year < 2020)

n <- sdf_nrow(selected_works)
# get 5000 rows
frac <- 5000 / n
frac

the_sample <- selected_works %>%
  sdf_sample(fraction = frac, replacement = FALSE,
             seed = 20220615) %>%
  collect()

the_sample %>%
  count(display_name, sort = TRUE)
# maybe it would be better to focus on specific disciplines. But we want to
# provide a general picture, so include it for now

# # A tibble: 19 × 2
#    display_name              n
#    <chr>                 <int>
#  1 Medicine               1430
#  2 Biology                 879
#  3 Chemistry               605
#  4 Computer science        505
#  5 Materials science       414
#  6 Psychology              287
#  7 Physics                 183
#  8 Environmental science   179
#  9 Geography               121
# 10 Business                 92
# 11 Sociology                88
# 12 Political science        81
# 13 Mathematics              63
# 14 Geology                  60
# 15 Art                      56
# 16 Philosophy               28
# 17 Economics                22
# 18 Engineering              14
# 19 History                   9

the_sample %>%
  count(author_position)
# slightly more first than last authorships
# # A tibble: 2 × 2
#   author_position     n
#   <chr>           <int>
# 1 first            2682
# 2 last             2434

the_sample %>%
  write_csv("data/processed/multilevel_sample.csv")

spark_disconnect(sc)
