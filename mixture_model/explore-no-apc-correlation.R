library(tidyverse)
library(brms)
library(broom)
library(broom.mixed)

df <- read_csv(here::here("data/processed/multilevel_sample.csv"))
wdi <- WDI::WDI_data$country %>%
  as_tibble() %>%
  select(iso2c, region, income)

base <- df %>%
  left_join(wdi, by = c("country_code" = "iso2c")) %>%
  select(institution_id, University, country, region, author_position,
         P_top10, field, country_code,
         APC_in_dollar, total_weight) %>%
  mutate(APC_in_dollar = case_when(is.na(APC_in_dollar) ~ 0,
                                   TRUE ~ APC_in_dollar)) %>%
  mutate(P_top10 = scale(log(P_top10), scale = FALSE))

pdf <- base %>%
  mutate(no_apc = if_else(APC_in_dollar == 0, TRUE, FALSE))


pdf %>%
  group_by(field) %>%
  summarise(cor = cor(P_top10, no_apc))



pdf %>%
  ggplot(aes(no_apc, P_top10)) +
  geom_boxplot() +
  facet_wrap(vars(field))


tdf <- pdf %>%
  filter(field == "Sociology")

get_weighted_cor <- function(df, var = no_apc) {
  weights <- df$total_weight

  df %>%
    select({{ var }}, P_top10) %>%
    cov.wt(wt = weights, cor = TRUE) %>%
    .$cor %>%
    .[[1, 2]]
}

pdf %>%
  group_by(field) %>%
  nest() %>%
  mutate(cor = map_dbl(data, get_weighted_cor)) %>%
  arrange(field)

pdf %>%
  filter(!no_apc) %>%
  group_by(field) %>%
  nest() %>%
  mutate(cor = map_dbl(data, get_weighted_cor, APC_in_dollar)) %>%
  arrange(field)


# there is no such correlation (higher resources, more "no apc" in Maths), at
# least not on a bivariate level
# let's try to explore the model further.


hm <- read_rds("bayes_model/final_models/17-brm-large-sample.rds.bz2")

conds <- expand_grid(field = c("Mathematics", "Environmental Science", "Sociology"),
             P_top10 = seq(-3, 3, .1)) %>%
  make_conditions(vars = c("field", "P_top10"))

conds <- expand_grid(field = c("Mathematics", "Environmental Science", "Sociology")) %>%
  make_conditions(vars = c("field"))

conds <- make_conditions(hm, vars = "field")

conditional_effects(hm, dpar = "theta2", conditions = conds)


# get proportion of papers that have no apc in fields
pdf %>%
  group_by(field) %>%
  count(no_apc) %>%
  mutate(prop = n / sum(n)) %>%
  filter(no_apc) %>%
  arrange(desc(prop))

# can we get to the same proportions from the model?
tidy_pars <- tidy(hm)

par1_hu <- tidy_pars %>%
  filter(str_detect(term, "^hu1")) %>%
  pull(estimate) %>%
  sum()
par1_hu

par2_hu <- tidy_pars %>%
  filter(str_detect(term, "^hu2")) %>%
  pull(estimate) %>%
  sum()
par2_hu

ran_vals <- tidy(hm, effects = "ran_vals")


ran_vals %>%
  filter(group == "field__hu1",
         level == "Sociology")
# the math for these calculations comes from: https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/#3-hurdle-lognormal-model
plogis(par1_hu + 2.07       + -0.351     ) - plogis(par1_hu + 2.07      )

# compute hu slopes for all fields
field_effects <- ran_vals %>%
  filter(group %in% c("field__hu1", "field__hu2")) %>%
  select(group, level, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(group_intercept =  `(Intercept)`) %>%
  mutate(population_intercept = case_when(group == "field__hu1" ~ par1_hu,
                                          TRUE ~ par2_hu),
         share_of_no_apc = plogis(population_intercept + group_intercept),
         ptop_effect = plogis(population_intercept + group_intercept + P_top10) - plogis(population_intercept + group_intercept))
field_effects

# These effects need to be interpreted together with theta, since mixing proportions
# differ between fields. Since this seems tricky to do exactly, I do not attempt it
# directly. But it shows, that Mathematics has the largest positive term for ptop_effect,
# which means that better resourced institutions from researchers publishing in
# this field do in fact publish more in non-APC journals.
# This is in contrast to the mere descriptive results.
# I'm inclined to stick with the model here, since it removes the effect of countries
# on this, whereas the bivariate correlations might be biased by country effects.
