library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(scales)
library(marginaleffects)

# Use the Johnson color palette
clrs <- MetBrewer::met.brewer("Johnson")

# Tell bayesplot to use the Johnson palette (for things like pp_check())
bayesplot::color_scheme_set(c("grey30", clrs[2], clrs[1], clrs[3], clrs[5], clrs[4]))

# theme adapted from https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide
theme_clean <- function(...) {
  theme_minimal(base_family = "Hind") +
    theme(strip.text = element_text(size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey90", color = NA),
          plot.margin = margin(5, 5, 5, 5))
}

extrafont::loadfonts(device = "win")


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


subsample <- base %>%
  filter(country == "Turkey")


subsample %>%
  ggplot(aes(APC_in_dollar)) +
  geom_density()

mix <- mixture(hurdle_lognormal(), hurdle_lognormal())

model_formula0 <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field),
  hu ~ 1 + P_top10 + (1 + P_top10|field)
)

model_formula <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field)
  # hu ~ 1 + P_top10 + (1 + P_top10|field),
  # theta2 ~ 1 + (1|field),
)



priors_0 <- c(
  prior(normal(7.5, 2), class = Intercept),
  prior(normal(0, .1), class = b),
  prior(normal(0, .1), class = b, dpar = hu),
  prior(normal(0, 1), class = sd),
  prior(normal(0, 1), class = sigma),
  prior(lkj(2), class = cor)
)


priors <- c(
  prior(normal(1, 2), class = Intercept, dpar = "mu1"),
  prior(normal(7.5, 2), class = Intercept, dpar = "mu2"),
  prior(normal(0, 1), class = b, dpar = "mu1"),
  prior(normal(0, 1), class = b, dpar = "mu2"),
  prior(normal(0, 1), class = sd, dpar = "mu1"),
  prior(normal(0, 1), class = sd, dpar = "mu2"),
  prior(normal(0, 1), class = sigma1),
  prior(normal(0, 1), class = sigma2),
  prior(lkj(2), class = cor)
)


no_mix <- brm(APC_in_dollar ~ 1 + P_top10 + (1 + P_top10|field),
              family = hurdle_lognormal(), data = subsample,
              seed = 1234, iter = 300, warmup = 150, sample_prior = "no",
              chains = 4,
              file = "bayes_model/no_mix")
summary(no_mix)
pp_check(no_mix) +
  coord_cartesian(xlim = c(0, 8000))

mix1 <- brm(model_formula,
                family = mix,
                prior = priors,
                data = subsample,
                seed = 1234,
            iter = 300, warmup = 150, file_refit = "on_change",
                file = "bayes_model/mix1")
pp_check(mix1, ndraws = 40) +
  coord_cartesian(xlim = c(0, 8000))
summary(mix1)
loo(no_mix, mix1)
# the mixture model fits the data way better, and can be justified:
# one process leads to 0 APC
# another type of publisher has low APC
# the third type of publisher have medium-high APCs
# effects of P_top might differ
#
# now: how to specify model for hu?

get_prior()


# mixture2 <- mixture(hurdle_lognormal(), gaussian())
#
# mix2 <- update(mix1, family = mixture2, file = "bayes_model/mix2")
# pp_check(mix2) +
#   coord_cartesian(xlim = c(0, 8000))
# loo(mix1, mix2)
# # gaussian addition is bad
#
# mixture3 <- mixture(hurdle_lognormal(), shifted_lognormal())
#
# mix3 <- update(mix1, family = mixture3, file = "bayes_model/mix3")
# # does not work, similar to lognormal, as there are "0"s in the response

model_formula1.1 <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field),
  hu1 ~ 1 + P_top10 + (1 + P_top10|field),
  hu2 ~ 1 + P_top10 + (1 + P_top10|field)
)

get_prior(model_formula1.1, data = subsample, family = mix)

mix2 <- update(mix1, formula. = model_formula1.1,
               file = "bayes_model/mix2")
pp_check(mix2) +
  coord_cartesian(xlim = c(0, 8000))
summary(mix2)

model_formula1.2 <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field),
  hu1 ~ 1 + P_top10 + (1 + P_top10|field),
  hu2 ~ 1 + P_top10 + (1 + P_top10|field),
  theta1 ~ 1 + (1|field)
)
mix3 <- update(mix1, formula. = model_formula1.2,
               file = "bayes_model/mix3")
pp_check(mix3, ndraws = 50) +
  coord_cartesian(xlim = c(0, 8000))
loo(mix1, mix2, mix3)

summary(mix3)

pred_vis <- function(df, model, country_selection, alpha = 1, ndraws = 1000) {
  get_back <- function(df) mutate(df, P_top10 = exp(6.09 + P_top10))

  df %>%
    filter(country == country_selection) %>%
    data_grid(P_top10, country, field) %>%
    add_predicted_draws(model, ndraws = ndraws, re_formula = NULL) %>%
    get_back() %>%
    ggplot(aes(P_top10, .prediction)) +
    stat_interval() +
    scale_color_manual(values = colorspace::lighten(clrs[4], c(.8, .67, .42))) +
    scale_y_continuous(labels = dollar) +
    geom_point(aes(y = APC_in_dollar), alpha = alpha,
               data = filter(df, country == country_selection) %>% get_back()) +
    facet_wrap(vars(field)) +
    labs(y = "Predicted vs. actual APC", x = expression(P["top 10%"]),
         color = "Credible interval") +
    # theme_minimal(base_family = "Hind") +
    theme_clean() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}
pred_vis(subsample, mix3, "Turkey", ndraws = 500)
# this works quite well


# now expand to two countries
subsample_larger <-   base %>%
  filter(country %in% c("Brazil", "Turkey")) %>%
  group_by(country) %>%
  slice_sample(n = 628) # sample for brazil

country_formula <- bf(
  APC_in_dollar | weights(total_weight) ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu1 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  hu2 ~ 1 + P_top10 + (1 + P_top10|field) + (1 + P_top10|country),
  theta1 ~ 1 + (1|field)
)

get_prior(country_formula, subsample_larger, family = mix)

country_priors <-   c(
    prior(normal(1, 2), class = Intercept, dpar = "mu1"),
    prior(normal(7.5, 2), class = Intercept, dpar = "mu2"),
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

mix_country <- brm(country_formula, data = subsample_larger, family = mix,
                   prior = country_priors, sample_prior = "no",
                   seed = 1234, iter = 500, warmup = 250,
                   file = "bayes_model/mix_country")
# chains are not mixing well. likely not strong enough priors for two countries
pp_check(mix_country) +
  coord_cartesian(xlim = c(0, 8000))
plot(mix_country)
pred_vis(subsample_larger, mix_country, "Brazil")

summary(mix_country)
ranef(mix_country)

# visualise predictions and compare
predictions_data <- predictions(
  mix_country,
  newdata = datagrid(P_top10 = seq(-3, 2, .1),
                     country = c("Turkey", "Brazil"),
                     field = "Biology")
)
predictions_data

predictions_data %>%
  mutate(ptop10 = exp(P_top10 + 6.09)) %>%
  ggplot(aes(x = ptop10, y = predicted, colour = country)) +
  geom_line(size = 1)


predictions_field <- predictions(
  mix_identified,
  newdata = datagrid(P_top10 = seq(-3, 2, .1),
                     country = c("Turkey"),
                     field = unique(base$field))
) %>%
  as_tibble()

predictions_field %>%
  mutate(ptop10 = exp(P_top10 + 6.09)) %>%
  ggplot(aes(x = ptop10, y = predicted, colour = field)) +
  geom_line(size = 1)

# very hard to interpret this
# but could be made to work with: show all lines, but only highlight 3-4
# (here: medicine, biology, environmental, one on the bottom)
# alternative: subtract all intercepts from start, and look at trajectory

p <- predictions_field %>%
  filter(P_top10 == 0) %>%
  select(field, offset = predicted) %>%
  left_join(predictions_field) %>%
  mutate(predicted = predicted - offset) %>%
  mutate(ptop10 = exp(P_top10 + 6.09)) %>%
  ggplot(aes(x = ptop10, y = predicted, colour = field)) +
  geom_line(size = 1)
p
plotly::ggplotly(p)

# even better: show it as a proportion
p <- predictions_field %>%
  filter(P_top10 == 0) %>%
  select(field, offset = predicted) %>%
  left_join(predictions_field) %>%
  mutate(predicted = predicted / offset) %>%
  mutate(ptop10 = exp(P_top10 + 6.09)) %>%
  ggplot(aes(x = ptop10, y = predicted, colour = field)) +
  geom_line(size = 1)
p
# this makes quite a bit of sense
# of course omits any uncertainty intervals
plotly::ggplotly(p)



predictions_country <- predictions(
  mix_identified,
  newdata = datagrid(P_top10 = seq(-3, 2, .1),
                     country = unique(subsample$country),
                     field = "Medicine")
) %>%
  as_tibble()


predictions_country_rescaled <- predictions_country %>%
  mutate(ptop10 = exp(P_top10 + 6.09))

pred_ctry_resc_center <- predictions_country %>%
  filter(P_top10 == 0) %>%
  select(country, offset = predicted) %>%
  left_join(predictions_country) %>%
  mutate(predicted = predicted / offset) %>%
  mutate(ptop10 = exp(P_top10 + 6.09))

p <- pred_ctry_resc_center %>%
  ggplot(aes(x = ptop10, y = predicted, group = country)) +
  geom_line(alpha = .2) +
  geom_line(size = 1, data = filter(pred_ctry_resc_center, country == "United States"), colour = "red") +
  geom_line(size = 1, data = filter(pred_ctry_resc_center, country == "Turkey"), colour = "blue") +
  geom_line(size = 1, data = filter(pred_ctry_resc_center, country == "Brazil"), colour = "green")
p
plotly::ggplotly(p)



# better here to use marginal effects?
field_comp <- comparisons(mix_identified, type = "response",
                          variables = list(P_top10 = 1),
                          newdata = datagrid(P_top10 = c(log(500) - 6.09,
                                                         log(2000) - 6.09),
                                             country = "United States",
                                             field = unique(subsample$field)))
field_comp <- field_comp %>%
  as_tibble()
field_comp

# need to do it separate, since going from 6->7 implies something else than going
# from 7->8 after exponentiating

loc1 <- log(500) - 6.09
field_comp1 <- comparisons(mix_identified, type = "response",
                           variables = list(P_top10 = (log(501) - 6.09) - (log(500) - 6.09)),
                           newdata = datagrid(P_top10 = loc1,
                                              country = "United States",
                                              field = unique(subsample$field)))

loc2 <- log(2000) - 6.09
field_comp2 <- comparisons(mix_identified, type = "response",
                           variables = list(P_top10 = (log(2001) - 6.09) - (log(2000) - 6.09)),
                           newdata = datagrid(P_top10 = loc2,
                                              country = "United States",
                                              field = unique(subsample$field)))

field_comp <- bind_rows(field_comp1, field_comp2) %>%
  as_tibble()
field_comp
# now we are there!!!

field_comp %>%
  mutate(ptop = exp(P_top10 + 6.09)) %>%
  ggplot(aes(comparison, xmin = conf.low, xmax = conf.high,
             y = fct_reorder2(field, ptop, comparison, first2))) +
  geom_pointrange() +
  facet_wrap(vars(ptop)) +
  labs(x = "% point change in APC for 1 unit change in P_top10")


field_comp %>%
  mutate(ptop = exp(P_top10 + 6.09)) %>%
  ggplot(aes(comparison, xmin = conf.low, xmax = conf.high,
             y = fct_reorder2(field, ptop, comparison, first2),
             colour = as.factor(ptop))) +
  geom_pointrange(position = position_dodge(width = .3)) +
  # facet_wrap(vars(ptop)) +
  labs(x = "% point change in APC for 1 unit change in P_top10")
# this is the result of adjusting the eps so that it represents one unit change
# across both conditions
# BUT CHECK. is this really the right scale?
# I think it is, given that it now fits the scale of the coefficients from
# `summary`, but we need to be sure here.
# check by inspecting the mu and hu parts separately, using the transformations
# vignette

field_pred <- predictions(
  mix_identified,
  newdata = datagrid(P_top10 = c(log(500) - 6.09,
                                 log(2000) - 6.09),
                     country = "United States",
                     field = unique(subsample$field))
) %>%
  as_tibble()

field_comp %>%
  left_join(field_pred, by = c("rowid", "type", "total_weight", "P_top10",
                               "country", "field")) %>%
  mutate(scale_factor = comparison / predicted,
         across(c(comparison, conf.low.x, conf.high.x), ~ .x * scale_factor),
         ptop = exp(P_top10 + 6.09)) %>%
  ggplot(aes(comparison, xmin = conf.low.x, xmax = conf.high.x,
             y = fct_reorder2(field, ptop, comparison, first2),
             colour = as.factor(ptop))) +
  geom_pointrange(position = position_dodge(width = .3)) +
  labs(x = "% change in APC for 1% change in P_top10")
# can the scale be correct? or is it rather

field_pred
field_comp









res <- subsample_larger %>%
  filter(country == "Turkey") %>%
  data_grid(P_top10, country, field) %>%
  add_epred_draws(mix_country, re_formula = NULL)

get_back <- function(df) mutate(df, P_top10 = exp(6.09 + P_top10))


res %>%
  get_back() %>%
  ggplot(aes(P_top10, .epred)) +
  stat_interval() +
  facet_wrap(vars(field))

res2 <- expand_grid(
  field = unique(base$field),
  P_top10 = c(-3, 2),
  country = "Turkey"
) %>%
  add_epred_draws(mix_country)

res2 %>%
  ggplot(aes(y = .epred, x = as_factor(P_top10))) +
  stat_halfeye() +
  facet_wrap(vars(field)) +
  coord_cartesian(ylim = c(0, 4000))


  scale_color_manual(values = colorspace::lighten(clrs[4], c(.8, .67, .42))) +
  scale_y_continuous(labels = dollar) +
  geom_point(aes(y = APC_in_dollar), alpha = alpha,
             data = filter(df, country == country_selection) %>% get_back()) +

  labs(y = "Predicted vs. actual APC", x = expression(P["top 10%"]),
       color = "Credible interval") +
  # theme_minimal(base_family = "Hind") +
  theme_clean() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

# try with four countries and prior on hu intercept
country_priors_intercept <-   c(
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
