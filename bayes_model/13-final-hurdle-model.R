library(tidyverse)
library(brms)

options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.file_refit = "on_change")

df <- read_csv("data/processed/multilevel_sample.csv")
wdi <- WDI::WDI_data$country %>%
  as_tibble() %>%
  select(iso2c, region, income)

base <- df %>%
  left_join(wdi, by = c("country_code" = "iso2c")) %>%
  select(institution_id, University, country, region, author_position,
         P_top10, field,
         APC_in_dollar, total_weight) %>%
  mutate(APC_in_dollar = case_when(is.na(APC_in_dollar) ~ 0,
                                   TRUE ~ APC_in_dollar)) %>%
  mutate(P_top10 = scale(log(P_top10), scale = FALSE))

model_formula <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|country) +
    (1 + P_top10|field),
  hu ~ 1 + P_top10 + (1 + P_top10|country) +
    (1 + P_top10|field)
)

priors <- c(
  prior(normal(7.5, 2), class = Intercept),
  prior(normal(0, 1), class = b),
  prior(normal(0, 1), class = b, dpar = hu),
  prior(normal(0, 1), class = sd),
  prior(normal(0, 1), class = sigma),
  prior(lkj(2), class = cor)
)

hm_final <- brm(model_formula,
                family = hurdle_lognormal(),
                prior = priors,
                data = base,
                seed = 1234,
                file = "bayes_model/final_models/hm_final")
