library(dplyr)
library(ggplot2)
library(patchwork)
library(brms)
library(marginaleffects)
library(gapminder)

# Build some 0s into the GDP column
set.seed(1234)
gapminder <- gapminder::gapminder %>%
  filter(continent != "Oceania") %>%
  # Make a bunch of GDP values 0
  mutate(prob_zero = ifelse(lifeExp < 50, 0.3, 0.02),
         will_be_zero = rbinom(n(), 1, prob = prob_zero),
         gdpPercap0 = ifelse(will_be_zero, 0, gdpPercap)) %>%
  select(-prob_zero, -will_be_zero)

mod <- brm(
  bf(gdpPercap0 ~ lifeExp,
     hu ~ lifeExp),
  data = gapminder,
  family = hurdle_lognormal(),
  chains = 1, cores = 1, warmup = 200, iter = 500, seed = 1234)

predictions_data <- predictions(
  mod,
  newdata = datagrid(lifeExp = seq(30, 80, 1)),
  dpar = "mu",
  transform_post = exp
)

slopes_data <- comparisons(
  mod,
  dpar = "mu",
  variables = list(lifeExp = eps),
  newdata = datagrid(lifeExp = seq(40, 80, 20)),
  transform_pre = function(hi, lo) ((exp(hi) - exp(lo)) / exp(eps)) / eps
) %>%
  left_join(predictions_data, by = "lifeExp") %>%
  # Point-slope formula: (y - y1) = m(x - x1)
  mutate(intercept = comparison * (-lifeExp) + predicted)

ggplot(predictions_data, aes(x = lifeExp, y = predicted)) +
  geom_line(size = 1) +
  geom_abline(data = slopes_data, aes(slope = comparison, intercept = intercept),
              size = 0.5, color = "red") +
  geom_point(data = slopes_data) +
  geom_label(data = slopes_data, aes(label = paste0("Slope: ", round(comparison, 1))),
             nudge_x = -1, hjust = 1) +
  theme_minimal()


# with log ################
gapminder2 <- gapminder %>%
  mutate(lifeExp = scale(log(lifeExp), scale = FALSE))

mod2 <- brm(
  bf(gdpPercap0 ~ lifeExp, #+ I(lifeExp^2),
     hu ~ lifeExp),
  data = gapminder2,
  family = hurdle_lognormal(),
  chains = 1, cores = 1, warmup = 200, iter = 500, seed = 1234)


offset <- attr(gapminder2$lifeExp, "scaled:center")

predictions_data <- predictions(
  mod2,
  newdata = datagrid(lifeExp = seq(-1, .5, .1)),
  dpar = "mu"
  # transform_post = exp
)
eps <- .01
slopes_data <- comparisons(
  mod2,
  dpar = "mu",
  variables = list(lifeExp = eps),
  newdata = datagrid(lifeExp = seq(-1, .5, .5)),
  type = "link"
  # transform_pre = function(hi, lo) ((exp(hi) - exp(lo)) / exp(eps)) / eps
) %>%
  left_join(predictions_data, by = "lifeExp") %>%
  # Point-slope formula: (y - y1) = m(x - x1)
  # mutate(lifeExp = exp(lifeExp + offset),
  mutate(intercept = comparison * (-lifeExp) + predicted)
slopes_data
slopes_data %>%
  mutate(lifeexpexp = exp(lifeExp + offset),
         predicted_exp = exp(predicted),
         predicted_gain = predicted_exp * (1 + comparison))

slopes2 <- marginaleffects()

predictions_data <- predictions_data %>%
  mutate(lifeExp = exp(lifeExp + offset))

ggplot(predictions_data, aes(x = lifeExp, y = predicted)) +
  geom_line(size = 1)
  geom_abline(data = slopes_data, aes(slope = comparison, intercept = intercept),
              size = 0.5, color = "red") +
  geom_point(data = slopes_data) +
  geom_label(data = slopes_data, aes(label = paste0("Slope: ", round(comparison, 1))),
             nudge_x = -1, hjust = 1) +
  theme_minimal()


predictions(
  mod2,
  newdata = datagrid(lifeExp = c(log(80) - offset, (log(80 * 1.01) - offset))),
  dpar = "mu"
  # transform_post = exp
) %>%
  mutate(pred = predicted - lag(predicted),
         predex = exp(predicted) / lag(exp(predicted)))
# this simply shows that we can recover the parameter by showing predictions at
# increases of 1%


# the coefficient in the log log model is simply the percentage interpretation
# this is the same across the range of lifeexp.it can be "back" transformed by
# applying it to varying levels of y, and potentially adjusting the change
# according to x (e.g., 50 -> 50.5 is 1%, so 50->51 is 2*b, but no, not really)

#>   rowid type    term       contrast comparison   conf.low  conf.high lifeExp
#> 1     1 link lifeExp (x + 0.01) - x 0.04245472 0.04079028 0.04410129    -1.0
#> 2     2 link lifeExp (x + 0.01) - x 0.04245472 0.04079028 0.04410129    -0.5
#> 3     3 link lifeExp (x + 0.01) - x 0.04245472 0.04079028 0.04410129     0.0
#> 4     4 link lifeExp (x + 0.01) - x 0.04245472 0.04079028 0.04410129     0.5

