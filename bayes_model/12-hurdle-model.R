library(tidyverse)
library(brms)
library(emmeans)         # Calculate marginal effects in fancy ways
library(marginaleffects)
library(tidybayes)       # Manipulate Stan objects in a tidy way
library(broom)
library(brmstools)
library(loo)
library(bayesplot)
library(gghalves)
library(modelr)
library(scales)

options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.file_refit = "on_change")

# Set some global Stan options
CHAINS <- 4
ITER <- 200
WARMUP <- ITER / 2
BAYES_SEED <- 1234

# Use the Johnson color palette
clrs <- MetBrewer::met.brewer("Johnson")

# Tell bayesplot to use the Johnson palette (for things like pp_check())
bayesplot::color_scheme_set(c("grey30", clrs[2], clrs[1], clrs[3], clrs[5], clrs[4]))

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
                                   TRUE ~ APC_in_dollar))


# visualise the two important variables
base %>%
  ggplot(aes(P_top10)) +
  geom_density()

base %>%
  ggplot(aes(APC_in_dollar)) +
  geom_density()
# both need logging, and the latter has a large amount of zeros

# prepare var
base <- base %>%
  mutate(P_top10 = scale(log(P_top10)))

subsample_countries <- c("United States", "Turkey", "Brazil", "Germany")
subsample_fields <- c("Biology", "Physics", "Sociology", "Medicine")

subsample <- base %>%
  filter(field %in% subsample_fields,
         country %in% subsample_countries)

# prior predictive checking
model_formula <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|country) +
    (1 + P_top10|field),
  hu ~ 1 + P_top10 + (1 + P_top10|country) +
    (1 + P_top10|field)
)

model_formula_nonzero <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|country) +
  (1 + P_top10|field)
)

priors <- c(prior(normal(7.5, .7), class = Intercept),
            prior(normal(0, .01), class = b),
            prior(normal(0, .01), class = b, dpar = hu),
            prior(normal(0, .2), class = sd),
            prior(normal(0, .1), class = sigma),
            prior(lkj(2), class = cor))

priors_wide <- c(
  prior(normal(7.5, 1), class = Intercept),
  prior(normal(0, .05), class = b),
  prior(normal(0, .05), class = b, dpar = hu),
  prior(normal(0, .5), class = sd),
  prior(normal(0, .5), class = sigma),
  prior(lkj(2), class = cor)
)

hm0 <- brm(model_formula,
           family = hurdle_lognormal(),
           prior = priors,
           data = subsample,
           chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
           sample_prior = "only",
           file = "bayes_model/final_models/hm0")

country_conditions <- make_conditions(
  tibble(country = c("Turkey", "China", "United States")),
  vars = c("country")
)

conditional_effects(hm0, conditions = country_conditions, re_formula = NULL)
conditional_effects(hm0, conditions = country_conditions,
                    re_formula = NULL, dpar = "hu")

hm1 <- update(hm0, sample_prior = "no", file = "bayes_model/final_models/hm1")
summary(hm1)
pp_check(hm1)

hm2 <- update(hm0, sample_prior = "no", newdata = base,
              iter = 2000, warmup = 1000, file = "bayes_model/final_models/hm2")
# 83 minutes
pp_check(hm2)
summary(hm2)

# other tries: censored model does not change anything (looic is actually worse)
# and we do not have a censoring mechanism.
# tried gaussian on data without zero component, but it is simply very bad
# (bimodal posterior)
#
# modeling without the zero component seems to give similar results for the
# non-zero part, but the zero-part is relevant and should stay in the model

# analyse and visualise -----
conditional_effects(hm2)
conditional_effects(hm2, dpar = "hu")
# wow, the error bars are wide

newdata <- expand_grid(country = c("China", "Germany", "United States", "Turkey",
                                   "Brazil"),
                       P_top10 = seq(-3, 3, by = .1),
                       field = c("Physics", "Biology", "Psychology", "Sociology",
                                 "Medicine", "Engineering"))
posterior_epred(hm2, newdata = newdata) %>% head()
tidy_epred <- hm2 %>%
  epred_draws(newdata = newdata)
tidy_epred

ptop_offset <- attributes(base$P_top10)$`scaled:center`
ptop_scale <- attributes(base$P_top10)$`scaled:scale`
tidy_epred <- tidy_epred %>%
  mutate(ptop_rescaled = exp(P_top10 * ptop_scale + ptop_offset))

tidy_epred %>%
  ggplot(aes(.epred, fill = country)) +
  stat_halfeye(alpha = .5) +
  facet_wrap(vars(field))

tidy_epred %>%
  ggplot(aes(ptop_rescaled, .epred)) +
  stat_lineribbon(color = clrs[4], size = 1) +
  scale_fill_manual(values = colorspace::lighten(clrs[4], c(0.95, 0.7, 0.4))) +
  facet_grid(rows = vars(country),
             cols = vars(field)) +
  scale_x_log10()
# this is going in a good direction, but having fields and countries at the same
# time is difficult

new_fields <- expand_grid(field = unique(base$field),
                          P_top10 = seq(-3, 3, by = .1),
                          country = "China")
field_draws <- hm2 %>%
  epred_draws(newdata = new_fields, ndraws = 500)
field_draws <- field_draws %>%
  mutate(ptop_rescaled = exp(P_top10 * ptop_scale + ptop_offset))

field_draws %>%
  ggplot(aes(ptop_rescaled, .epred)) +
  stat_lineribbon(color = clrs[4], size = 1) +
  scale_fill_manual(values = colorspace::lighten(clrs[4], c(0.95, 0.7, 0.4))) +
  facet_wrap(vars(field)) +
  scale_x_log10()
# this is interesting, but need to choose reference category for country
# so directly showing coefficients would be better

new_countries <- expand_grid(field = "Medicine",
                          P_top10 = seq(-3, 3, by = .1),
                          country = unique(base$country))
country_draws <- hm2 %>%
  epred_draws(newdata = new_countries, ndraws = 500) %>%
  mutate(ptop_rescaled = exp(P_top10 * ptop_scale + ptop_offset))

country_draws %>%
  ggplot(aes(ptop_rescaled, .epred)) +
  stat_lineribbon(color = clrs[4], size = 1) +
  scale_fill_manual(values = colorspace::lighten(clrs[4], c(0.95, 0.7, 0.4))) +
  facet_wrap(vars(country)) +
  scale_x_log10()
# this does not make it easy to show anything
# use the code from here: https://discourse.mc-stan.org/t/convenience-function-for-plotting-random-group-effects/13461/2

get_variables(hm2) %>% head(100)

hm2 %>%
  spread_draws(r_field[field,term]) %>%
  median_qi() %>%
  filter(term == "P_top10") %>%
  arrange(r_field)
  head()

hm2 %>%
  spread_draws(b_Intercept, b_P_top10, r_field[field,term])

# field effects
rescale_ptop <- function(x) exp(x * ptop_scale + ptop_offset)
hm2 %>%
  spread_draws(b_P_top10, r_field[field,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_P_top10 + r_field,
         total_effect = rescale_ptop(total_effect)) %>%
  ggplot(aes(x = total_effect, y = reorder(field, total_effect))) +
  stat_halfeye() +
  labs(x = "Average marginal effect of what exactly?")

# country effects
hm2 %>%
  spread_draws(b_P_top10, r_country[country,term]) %>%
  filter(term == "P_top10") %>%
  mutate(total_effect = b_P_top10 + r_country,
         total_effect = rescale_ptop(total_effect)) %>%
  ggplot(aes(x = total_effect, y = reorder(country, total_effect))) +
  stat_halfeye() +
  labs(x = "Average marginal effect of what exactly?")

# need the same for hu part

# visualise intercepts-----
# countries
hm2 %>%
  spread_draws(b_Intercept, r_country[country,term]) %>%
  filter(term == "Intercept") %>%
  mutate(country_intercept = exp(b_Intercept + r_country)) %>%
  ggplot(aes(x = country_intercept, reorder(country, country_intercept))) +
  stat_pointinterval() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Intercept for APC", y = NULL)
# this is correct now, but not very informative, because it gives the intercept
# at the mean of the logged ptop10

# hurdle component
hm2 %>%
  spread_draws(b_hu_Intercept, r_country__hu[country,term]) %>%
  filter(term == "Intercept") %>%
  mutate(country_intercept = exp(b_hu_Intercept + r_country__hu)) %>%
  ggplot(aes(x = country_intercept, reorder(country, country_intercept))) +
  stat_pointinterval() +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Proportion of articles with no APC (at mean of logged P_top10)",
       y = NULL)

summary(hm2)


# visualise predictions for certain countries
