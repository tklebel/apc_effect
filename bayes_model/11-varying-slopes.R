library(tidyverse)
library(brms)
library(emmeans)         # Calculate marginal effects in fancy ways
library(tidybayes)       # Manipulate Stan objects in a tidy way
library(broom)
library(brmstools)

options(mc.cores = 4,
        brms.backend = "cmdstanr")

# Set some global Stan options
CHAINS <- 4
ITER <- 2000
WARMUP <- 1000
BAYES_SEED <- 1234

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
base <- df %>%
  select(institution_id, country, author_position, PP_top10,
         field = display_name,
         APC_in_dollar)

model_base <- base %>%
  filter(APC_in_dollar != 0) %>%
  mutate(PP_top10 = scale(log(PP_top10)),
         APC_in_dollar = scale(APC_in_dollar))

# start with model that incorporates all varying intercepts
m1 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1|country) + (1|institution_id) +
            (1|author_position) + (1|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          control = list(adapt_delta = .95),
          save_model = "bayes_model/m1.stan",
          file = "bayes_model/m1")
summary(m1)
ranef(m1)
launch_shinystan(m1)


# and now let intercepts vary by country and field
m2 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
            (1|author_position) + (1 + PP_top10|field),
          family = "gaussian",
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = model_base,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          control = list(adapt_delta = .98),
          save_model = "bayes_model/m2.stan",
          file = "bayes_model/m2")
# 76 divergent transitions,
# maybe increase adapt delta even a little further, and potentially warm up
# longer
summary(m2)
ranef(m2)

# very interesting: negative correlation between intercept and pptop10 in
# countries: so for countries paying high APCs, pptop has a negative effect
# but positive correlation for fields: low APC, low effect, high APC, high
# impact of PP_top10
#
# Overall much more variance between fields than countries.
#
# overall positive effect of PP_top10. "negative" values within fields and
# countries need to be taken into context here
#
# todos:
# - rerun with higher adapt_delta and longer warm-up
# - visualise draws for fields and for countries -> there maybe link to regions
# (income or continent)
# - check with a naive model that models PP_top10 simply per country (no pooling)
# -

# look at visualising
res <- as_draws_df(m2, variable = "PP_top10", regex = TRUE)

forest(m2, "field") # very interesting
ggsave("bayes_model/plots/random_effect_fields.png", width = 14, height = 6)
# this shows the effects after combining all parts, i.e., including group and
# population level effects
forest(m2, "country")

# here try to find an equivalent with tidybayes

# general parameter
res %>%
  ggplot(aes(b_PP_top10)) +
  geom_density()

conditional_fields <- fields %>%
  select(field = display_name_indicator)

conditional_effects(m2, conditions = conditional_fields)

waic(m1, m2)


# issues with the data: high numbers of 0 values
# this might help a lot: https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/

# do pp check first
pp_check(m2)
# well, this shows the problem, doesn't it? ^^
# no adapt_delta will get us out of this mis-specification

base_small <- base %>%
  slice_sample(n = 1000) %>%
  mutate(PP_top10 = scale(PP_top10))

# hurdle model
m3 <- brm(bf(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
            (1|author_position) + (1 + PP_top10|field),
            hu ~ 1),
          family = hurdle_lognormal(),
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = base_small,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          control = list(adapt_delta = .8),
          save_model = "bayes_model/m3.stan",
          file = "bayes_model/m3")
# this samples much faster, despite the low adapt delta
pp_check(m3)
# this is already way better, although not yet very good
summary(m3)
ranef(m3)
forest(m3, "field")
forest(m3, "country")

# more complex model for hurdle
m5 <- brm(bf(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
               (1|author_position) + (1 + PP_top10|field),
             hu ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
               (1|author_position) + (1 + PP_top10|field)),
          family = hurdle_lognormal(),
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = base_small,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          control = list(adapt_delta = .9),
          save_model = "bayes_model/m5.stan",
          file = "bayes_model/m5")
pp_check(m5, ndraws = 30)
summary(m5)
forest(m5, "field")
forest(m5, "country")

conditions_base <- expand_grid(field = c("Biology", "Medicine", "Psychology"),
                               country = c("China", "United States", "United Kingdom"))
conditions <- make_conditions(conditions_base, vars = c("field", "country"))

conditional_effects(m5, conditions = conditions, re_formula = NULL) %>%
  plot(ncol = 3)

# hurdle part: predicted proportion of zero APC
conditional_effects(m5, conditions = conditions, re_formula = NULL, dpar = "hu") %>%
  plot(ncol = 3)

fixef(m5) # hurdle modle is on logit scale
ranef(m5)

# lets try one more time, this time with logged pptop10
base_big <- base %>%
  mutate(PP_top10 = scale(log(PP_top10)))


m6 <- brm(bf(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
               (1|author_position) + (1 + PP_top10|field),
             hu ~ 1 + PP_top10 + (1 + PP_top10|country) + (1|institution_id) +
               (1|author_position) + (1 + PP_top10|field)),
          family = hurdle_lognormal(),
          prior = c(prior(normal(0, .5), class = Intercept),
                    prior(normal(0, .5), class = b),
                    prior(exponential(1), class = sd),
                    prior(exponential(1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = base_big,
          chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
          control = list(adapt_delta = .9),
          save_model = "bayes_model/m6.stan",
          file = "bayes_model/m6")

# All 4 chains finished successfully.
# Mean chain execution time: 15196.8 seconds.
# Total execution time: 15308.7 seconds.
#
# Warning: 4000 of 4000 (100.0%) transitions hit the maximum treedepth limit of 10.
# See https://mc-stan.org/misc/warnings for details.

# this hurdle thing seems to be a little unwieldy. maybe better try with separate
# models, one for APC == 0 (glm), and one for APC > 0.
# and for this try a completely nested formulation
# 1 + PP_top10 + (1 + PP_top10 | author_position / institution_id / country) + (1 + PP_top10|field)

