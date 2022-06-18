library(tidyverse)
library(brms)
library(emmeans)         # Calculate marginal effects in fancy ways
library(tidybayes)       # Manipulate Stan objects in a tidy way
library(broom)
library(brmstools)
library(loo)
library(bayesplot)

options(mc.cores = 4,
        brms.backend = "cmdstanr",
        brms.file_refit = "on_change")

# Set some global Stan options
CHAINS <- 4
ITER <- 200
WARMUP <- 100
BAYES_SEED <- 1234

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
base <- df %>%
  select(institution_id, country, author_position, PP_top10,
         field = display_name,
         APC_in_dollar)

subsample_countries <- c("China", "United States", "Turkey",
                         "Russia", "Germany", "United Kingdom")
subsample_fields <- c("Biology", "Physics", "Sociology", "Psychology")

model_base <- base %>%
  filter(APC_in_dollar != 0) %>%
  mutate(PP_top10 = scale(log(PP_top10)),
         APC_in_dollar = log(APC_in_dollar)) %>%
  filter(country %in% subsample_countries,
         field %in% subsample_fields)

# start with very simple model: institutions nested in countries
m1 <- brm(APC_in_dollar ~ 1 + (1|country / institution_id),
          family = lognormal(),
          prior = c(prior(normal(0, .5), class = Intercept),
                    # prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          # control = list(adapt_delta = .95),
          save_model = "bayes_model/m1.stan",
          file = "bayes_model/m1"
          )

pp_check(m1)
# this is still a bad fit for the data - large parts of the original data are
# not fitted properly, and the treedepth is hit for all transitions

# does having country and institution separate make a difference?
m1.1 <- brm(APC_in_dollar ~ 1 + (1|country) + (1|institution_id),
          family = lognormal(),
          prior = c(prior(normal(0, .5), class = Intercept),
                    # prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          # control = list(adapt_delta = .95),
          save_model = "bayes_model/m1.1.stan",
          file = "bayes_model/m1.1")
pp_check(m1.1)
loo(m1, m1.1)
# m1 is better in terms of pareto diagnostics

# add field and author completely nested
m2 <- brm(APC_in_dollar ~ 1 + (1|field / country / author_position) + (1|country / institution_id),
          family = lognormal(),
          prior = c(prior(normal(0, .1), class = Intercept),
                    # prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          # control = list(adapt_delta = .95),
          save_model = "bayes_model/m2.stan",
          file = "bayes_model/m2")
# still the same warning of treedepth
pp_check(m2)
summary(m2)
ranef(m2)
forest(m2)
forest(m2, "field")
# something is off, the posterior is bimodal

m3 <- brm(APC_in_dollar ~ 1 + (1|country),
          family = lognormal(),
          prior = c(prior(normal(0, .1), class = Intercept),
                    # prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          save_model = "bayes_model/m3.stan",
          file = "bayes_model/m3")
pp_check(m3)
summary(m3)
# finally, no treedepth. so I might have overspecified this
# one issue of the sample might be, that we sample across all institutions
# we should at least sample full institutions within countries
# but this might give much less data, since we have many observations per
# institution

m4 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country),
          family = gaussian(),
          prior = c(prior(normal(7, 1), class = Intercept),
                    prior(normal(.5, .2), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          sample_prior = "only",
          save_model = "bayes_model/m4.stan",
          file = "bayes_model/m4")
# prior predictive check
conditions <- make_conditions(tibble(country = "Atlantis"), vars = c("country"))
conditional_effects(m4, conditions = conditions, re_formula = NULL)
# so the prior assumes that APC is high for low pptop?
# that is not right
#
# # try to get lognormal prior to work
model_base_exp <- model_base %>%
  mutate(APC_in_dollar = exp(APC_in_dollar))

m5 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country),
          family = lognormal(),
          prior = c(prior(normal(7.5, .7), class = Intercept),
                    prior(normal(.2, .01), class = b),
                    prior(normal(0, .2), class = sd),
                    prior(normal(0, .1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = model_base_exp,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          sample_prior = "only")
# prior_summary(m5)
conditional_effects(m5, conditions = conditions, re_formula = NULL)
# these work well, as informative priors, constraining the potential space, but
# within reasonable bounds


# now we can use those priors in effect
m5.1 <- update(m5, sample_prior = "no", iter = 1000, warmup = 500,
               control = list(adapt_delta = .9))

pp_check(m5.1, ndraws = 30)
summary(m5.1)
forest(m5.1)

