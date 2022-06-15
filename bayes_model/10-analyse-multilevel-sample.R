library(tidyverse)
library(rethinking)

df <- read_csv("data/processed/multilevel_sample.csv")

# make model df
mdf <- df %>%
  select(institution_id, country, author_position, PP_top10, display_name,
         APC_in_dollar)

# make indicator variables out of institution and country
mdfi <- mdf %>%
  mutate(across(c(institution_id, country), ~as.factor(.x) %>% as.numeric()))
mdfi

count(mdf, country, sort = TRUE)
count(mdfi, country, sort = TRUE)

# for simplicity (adherence to the simulated model), let's use one position and
# one discipline for now, before adding further complexity
mdfi_small <- mdfi %>%
  filter(author_position == "first", display_name == "Medicine",
         !is.na(APC_in_dollar)) %>%
  # try redoing the indicator variable construction (maybe they need to run from
  # 1:n, and not have gaps in them)
  mutate(across(c(institution_id, country), ~as.factor(.x) %>% as.numeric()))

dlist <- list(
  country = mdfi_small$country,
  institute = mdfi_small$institution_id,
  inst_res = mdfi_small$PP_top10,
  apc_price = mdfi_small$APC_in_dollar
)

m1 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma + b_r * inst_res,
    b_r ~ dnorm(0, 1),
    z[country] ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, 1),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
traceplot_ulam(m1)
trankplot(m1)

precis(m1)
#           mean     sd   5.5%   94.5% n_eff Rhat4
# b_r       0.01   0.95  -1.56    1.61  2683  1.00
# a_sigma 321.50 150.08   0.42  421.86     2  4.14
# b_bar     0.64   1.32  -1.25    3.48    11  1.26
# b_sigma   1.15   1.05   0.07    2.78    54  1.07
# sigma   821.40 257.38 654.36 1293.68     2 16.99

# this model behaved very badly, it is basically all over the place.
# why? is there an issue in terms of sparse data? so maybe we have only very
# few data points per university per country. if this is the case, we would need
# nested sampling: only a couple of countries, and from those, a couple of
# institutions, and from those then sampled works.
#
# What might other reasons be? Maybe the non-centered parameterization is bad for
# this particular data? (unlikely)
#
# maybe something else is different to the simulated data?

# investigate the first one
mdfi_small %>%
  count(country, sort = TRUE)

mdfi_small %>%
  count(country, institution_id, sort = TRUE)

mdfi_small %>%
  count(country, institution_id, sort = TRUE) %>%
  count(country)

# there seems to be sufficient data, so the problem must be somewhere else.
# maybe build the model incrementally: start with naive linear regression, then
# have country and institutions separately, and then try together.
