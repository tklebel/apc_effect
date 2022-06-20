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

options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.file_refit = "on_change")

# Set some global Stan options
CHAINS <- 4
ITER <- 200
WARMUP <- ITER / 2
BAYES_SEED <- 1234

df <- read_csv("data/processed/multilevel_sample.csv")

base <- df %>%
  select(institution_id, University, country, author_position, P_top10, field,
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

subsample_countries <- c("China", "United States", "Turkey", "Brazil",
                         "Russia", "Germany")
subsample_fields <- c("Biology", "Physics", "Sociology", "Medicine")

set.seed(98734)
subsample <- base %>%
  filter(field %in% subsample_fields,
         country %in% subsample_countries) %>%
  sample_n(size = 1000)

# prior predictive checking
model_formula <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|country) +
    (1 + P_top10|field),
  hu ~ 1 + P_top10 + (1 + P_top10|country) + (1 + P_top10|field)
)

priors <- c(prior(normal(7.5, .7), class = Intercept),
            prior(normal(0, .01), class = b),
            prior(normal(0, .01), class = b, dpar = hu),
            prior(normal(0, .2), class = sd),
            prior(normal(0, .1), class = sigma),
            prior(lkj(2), class = cor))

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
