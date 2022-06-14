Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 5 # this should always stay the same
config$spark.executor.instances <- 5 # this can go up to 27, depending on RAM
config$spark.executor.memory <- "12G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "OA_APCs")

spark_read_parquet(
  sc, "works", "/user/tklebel/apc_paper/all_papers_selected_cols.parquet",
  memory = TRUE
)
works <- tbl(sc, "works")

mean_apcs <- works %>%
  group_by(University, publication_year, country, PP_top10) %>%
  # compute the average APC using fractional authorships as weights
  mutate(sum_frac = sum(work_frac)) %>%
  group_by(University, publication_year, country, PP_top10, sum_frac,
           author_position) %>%
  summarise(mean_apc = sum(work_frac * APC_in_dollar) / sum_frac)

mean_apcs_local <- mean_apcs %>%
  collect()

mean_apcs_local %>%
  filter(publication_year == 2019) %>%
  View()

# plot for 2019
apc_2019 <- mean_apcs_local %>%
  filter(publication_year == 2019)

labels <- apc_2019 %>%
  group_by(author_position) %>%
  summarise(cor = cor(mean_apc, PP_top10)) %>%
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

apc_2019 %>%
  ggplot(aes(PP_top10, mean_apc)) +
  geom_point(aes(colour = sum_frac),
             alpha = .5) +
  geom_smooth() +
  facet_wrap(vars(author_position)) +
  geom_text(data = labels, aes(label = cor, x = .25, y = 2250)) +
  scale_colour_viridis_c(trans = "sqrt")
ggsave(filename = "plots/apc_pptop10_2019.png", width = 6, height = 4.5)

# might want to add world bank income regions and geographic regions

# now display over time
mean_apcs_local %>%
  group_by(publication_year) %>%
  mutate(pptop10_quantiles = cut_quartiles(PP_top10)) %>%
  group_by(pptop10_quantiles, publication_year) %>%
  summarise(mean_apc = weighted.mean(mean_apc, sum_frac, na.rm = TRUE)) %>%
  ggplot(aes(publication_year, mean_apc, colour = pptop10_quantiles,
             group = pptop10_quantiles)) +
  geom_line()
ggsave(filename = "plots/apc_pptop10_09_19.png", width = 7, height = 4.5)


# todos:
# - first and last authors
# - comparisons across continent and income region
# - comparison across fields
# - multilevel model (how to incoroporate time?)
#   important here: do it on individual article level, since there might be a
#   lot of variation within universities, that is lost in just looking at means

spark_disconnect(sc)
