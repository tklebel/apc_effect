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
WARMUP <- 100
BAYES_SEED <- 1234

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
base <- df %>%
  select(institution_id, country, author_position, PP_top10,
         field = display_name,
         APC_in_dollar)

subsample_countries <- c("China", "United States", "Turkey", "Brazil",
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

# expand by adding fields and author positions
m6 <- brm(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) +
            (1 + PP_top10|field) + (1 + PP_top10|author_position),
          family = lognormal(),
          prior = c(prior(normal(7.5, .7), class = Intercept),
                    prior(normal(0, .01), class = b),
                    prior(normal(0, .2), class = sd),
                    prior(normal(0, .1), class = sigma),
                    prior(lkj(2), class = cor)),
          data = model_base_exp,
          chains = CHAINS, iter = 600, warmup = 300, seed = BAYES_SEED,
          sample_prior = "no", file = "bayes_model/m6")
pp_check(m6, ndraws = 30)
summary(m6)

forest(m6, "country")
forest(m6, "field")
forest(m6, "author_position")

# use more fields
subsample_countries <- c("China", "United States", "Turkey", "Brazil",
                                     "Russia", "Germany", "United Kingdom")

new_base <- base %>%
  filter(APC_in_dollar != 0) %>%
  mutate(PP_top10 = scale(log(PP_top10))) %>%
  filter(country %in% subsample_countries)

m6.1 <- update(m6, newdata = new_base, file = "bayes_model/m6.1",
               control = list(adapt_delta = .9))
pp_check(m6.1)
summary(m6.1)
forest(m6.1, "country")
forest(m6.1, "field")
forest(m6.1, "author_position")
ranef(m6.1)

# what sample sizes do we have in this data?
new_base %>%
  count(field, sort = TRUE)
new_base %>%
  count(country, sort = TRUE)
new_base %>%
  count(institution_id, sort = TRUE)

# try to reduce complexity by removing the author term
m6.2 <- update(m6.1, . ~ . -(1 + PP_top10|author_position),
               file = "bayes_model/m6.2")
# this samples better, since it presumably reduces issues with identification
summary(m6.2)
forest(m6.2, "country")
forest(m6.2, "field")

# visualise marginal effects
country_conditions <- make_conditions(
  tibble(country = c("Turkey", "China", "United States"),
         field = c("Biology", "Medicine", "Environmental science")),
  vars = c("country", "field")
)

conditional_effects(m6.2, conditions = country_conditions, re_formula = NULL)


newdata <- expand_grid(country = c("China", "Germany", "United States", "Turkey"),
                       PP_top10 = seq(-2, 2, by = .1),
                       field = c("Medicine", "Biology", "Environmental science"))

posterior_epred(m6.2, newdata = newdata) %>% head()
tidy_epred <- m6.2 %>%
  epred_draws(newdata = newdata)
tidy_epred

tidy_epred %>%
  ggplot(aes(.epred, fill = country)) +
  stat_halfeye(alpha = .5) +
  facet_wrap(vars(field))

tidy_epred %>%
  ungroup() %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(PP_top10, .epred, colour = country)) +
  geom_point() +
  facet_wrap(vars(field))

tidy_epred %>%
  ggplot(aes(PP_top10, .epred, colour = country)) +
  geom_smooth() +
  facet_wrap(vars(field))
# this looks like the sort of plot we are after

# for comparison, let us do the predicted values (incorporating full uncertainty)
tidy_pred <- m6.2 %>%
  predicted_draws(newdata = newdata)

tidy_pred %>%
  # ungroup() %>%
  # slice_sample(n = 50000) %>%
  ggplot(aes(PP_top10, .prediction, colour = country)) +
  geom_smooth() +
  facet_wrap(vars(field))
# when visualising this way, there is no difference in the slope and intercept
# of the lines (as expected), simply the uncertainty interval is bigger
#
# what I don't really get about those: why is the line so curved up, when the
# parameter itself is 0?
# looking at ranef(m6.2): because all the effect lives now in the country
# coefficient - there is simply no further variation that goes unexplained
# right now

# try with marginaleffects/eamms
countries_to_use <- tibble(country = c("Turkey", "China", "United States"))
m6.2preds <- m6.2 %>%
  predictions(newdata = datagrid(PP_top10 = seq(-3, 3, .1),
                                 country = countries_to_use))

  emmeans(~ PP_top10 + country, var = "PP_top10",
          at = list(PP_top10 = seq(-3, 3, .1),
                    country = countries_to_use),
          regrid = "response") |>
  as_tibble()

ggplot(logit_predictions, aes(x = public_sector_corruption, y = prob, color = region)) +
  geom_line(size = 1) +
  labs(x = "Public sector corruption", y = "Predicted probability of having\na campaign finance disclosure law", color = NULL) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c(clrs, "grey30")) +
  theme_mfx() +
  theme(legend.position = "bottom")


# give hurdle model another try
hurdle_base <- base %>%
  mutate(PP_top10 = scale(log(PP_top10))) %>%
  filter(country %in% subsample_countries,
         field %in% subsample_fields)

hm1 <- brm(bf(APC_in_dollar ~ 1 + PP_top10 + (1 + PP_top10|country) +
             (1 + PP_top10|field),
             hu ~ 1 + PP_top10 + (1 + PP_top10|country) +
               (1 + PP_top10|field)),
           family = hurdle_lognormal(),
           prior = c(prior(normal(7.5, .7), class = Intercept),
                     prior(normal(0, .01), class = b),
                     prior(normal(0, .01), class = b, dpar = hu),
                     prior(normal(0, .2), class = sd),
                     prior(normal(0, .1), class = sigma),
                     prior(lkj(2), class = cor)),
           data = hurdle_base,
           chains = CHAINS, iter = 200, warmup = 100, seed = BAYES_SEED,
           sample_prior = "no", file = "bayes_model/hm1")
pp_check(hm1)
summary(hm1)
forest(hm1, "country")
forest(hm1, "field")

country_conditions <- make_conditions(
  tibble(country = c("Turkey", "China", "United States")),
  vars = c("country")
)

conditional_effects(hm1, conditions = country_conditions, re_formula = NULL)
conditional_effects(hm1, conditions = country_conditions,
                    re_formula = NULL, dpar = "hu")
# this model seems to work now, but it obvs samples slower than the other one,
# simply because it is actually two models.
# The conditional plots show that although the coefficients are small, the
# conditional effects (here: the expected APC?) is quite substantial: going from
# mean pptop10 two sd's up increases the average APC by about 300-400$
# Given that these coefficients are bigger for other countries, this might be
# massive

# do the same epred as above
newdata <- expand_grid(country = c("China", "Germany", "United States", "Turkey"),
                       PP_top10 = seq(-2, 2, by = .1),
                       field = c("Physics", "Biology", "Psychology", "Sociology"))
posterior_epred(hm1, newdata = newdata) %>% head()
tidy_epred <- hm1 %>%
  epred_draws(newdata = newdata)
tidy_epred

tidy_epred %>%
  ggplot(aes(.epred, fill = country)) +
  stat_halfeye(alpha = .5) +
  facet_wrap(vars(field))

tidy_epred %>%
  ungroup() %>%
  slice_sample(n = 1000) %>%
  ggplot(aes(PP_top10, .epred, colour = country)) +
  geom_point() +
  facet_wrap(vars(field))

tidy_epred %>%
  ggplot(aes(PP_top10, .epred, colour = country)) +
  geom_smooth() +
  facet_wrap(vars(field))
# both of them are very interesting, and bring out differences regarding Turkey
# (which might be an artefact of using small data): in the hurdle model Turkey
# is much flatter, and much lower (because it has way more 0s then the others)
