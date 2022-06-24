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

set.seed(1234)
subsample <- base %>%
  slice_sample(n = 15000)

# family
mix <- mixture(hurdle_lognormal(), hurdle_lognormal(), order = TRUE)

# formula
model_formula  <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu1 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu2 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  theta1 ~ 1 + (1|field)
)

priors <- c(
  prior(normal(5, 2), class = Intercept, dpar = "mu1"),
  prior(normal(7.5, 2), class = Intercept, dpar = "mu2"),
  prior(normal(0, 2), class = Intercept, dpar = "hu1"),
  prior(normal(0, 2), class = Intercept, dpar = "hu2"),
  prior(normal(0, 1), class = b, dpar = "mu1"),
  prior(normal(0, 1), class = b, dpar = "mu2"),
  prior(normal(0, 1), class = b, dpar = "hu1"),
  prior(normal(0, 1), class = b, dpar = "hu2"),
  prior(normal(0, 1), class = sd, dpar = "mu1"),
  prior(normal(0, 1), class = sd, dpar = "mu2"),
  prior(normal(0, 1), class = sd, dpar = "hu1"),
  prior(normal(0, 1), class = sd, dpar = "hu2"),
  prior(normal(0, 1), class = sigma1),
  prior(normal(0, 1), class = sigma2),
  prior(lkj(2), class = cor)
)

mix_extensive <- brm(model_formula,
                     family = mix,
                     prior = priors,
                     data = subsample,
                     seed = 1234,
                     file = "bayes_model/final_models/mix_extensive")
# start: 19:53
# end: 219m

pp_check(mix_extensive) +
  coord_cartesian(xlim = c(0, 8000))


mix_adapt <- brm(model_formula,
                 family = mix,
                 prior = priors,
                 data = subsample,
                 seed = 1234,
                 control = list(adapt_delta = .95),
                 file = "bayes_model/final_models/mix_extensive_adapt")
# start 05:30
# 266m
summary(mix_adapt)
# model did not converge, probably an issue of identifiability
pp_check(mix_adapt) +
  coord_cartesian(xlim = c(0, 8000))
# posterior looks good though

# rhat was actually terrible with the first version too
launch_shinystan(mix_adapt)
# the issue is clear: one chain had the order of mu1 reversed


# use narrower priors to identify the two mu components, and set init to "0",
# as indicated in the example on ?mixture (Which should help ensure that mu2
# is estimated being higher as mu1)
# also set ae prior on theta, just to make sure it does not take unreasonable
# values
priors_narrower <- c(
  prior(normal(5, .5), class = Intercept, dpar = "mu1"),
  prior(normal(7.5, .5), class = Intercept, dpar = "mu2"),
  prior(normal(0, .5), class = Intercept, dpar = "theta1"),
  prior(normal(0, 2), class = Intercept, dpar = "hu1"),
  prior(normal(0, 2), class = Intercept, dpar = "hu2"),
  prior(normal(0, 1), class = b, dpar = "mu1"),
  prior(normal(0, 1), class = b, dpar = "mu2"),
  prior(normal(0, 1), class = b, dpar = "hu1"),
  prior(normal(0, 1), class = b, dpar = "hu2"),
  prior(normal(0, 1), class = sd, dpar = "mu1"),
  prior(normal(0, 1), class = sd, dpar = "mu2"),
  prior(normal(0, 1), class = sd, dpar = "hu1"),
  prior(normal(0, 1), class = sd, dpar = "hu2"),
  prior(normal(0, 1), class = sigma1),
  prior(normal(0, 1), class = sigma2),
  prior(lkj(2), class = cor)
)

mix_identified <- brm(model_formula,
                      family = mix,
                      prior = priors_narrower,
                      data = subsample,
                      seed = 1234,
                      control = list(adapt_delta = .9),
                      init = 0,
                      file = "bayes_model/final_models/mix_identified")
# 10:30; 280 minutes
summary(mix_identified)
# this worked out great, no warnings and good rhats, good ESS

pp_check(mix_identified) +
  coord_cartesian(xlim = c(0, 8000))

# visualise prior
tibble(x = seq(2, 9, .1)) %>%
  mutate(mu1 = dnorm(x, 5, .5),
         mu2 = dnorm(x, 7.5, .5)) %>%
  pivot_longer(contains("mu")) %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x, value, colour = name)) +
  geom_line()

launch_shinystan(mix_identified)

ranef(mix_identified)
