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
  inst_res = standardize(mdfi_small$PP_top10),
  apc_price = standardize(mdfi_small$APC_in_dollar)
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
#          mean   sd  5.5% 94.5% n_eff Rhat4
# b_r      0.23 0.06  0.14  0.32  1392     1
# a_sigma  0.21 0.04  0.15  0.29   641     1
# b_bar   -0.09 0.05 -0.16 -0.02   779     1
# b_sigma  0.06 0.04  0.01  0.12   371     1
# sigma    0.45 0.01  0.43  0.47  1656     1

# there is substantial variation between countries, but it seems most variation
# within countries (between institutions) can be explained with the prestige
# proxy
#
# need to further diagnose the model, as well as to get an understanding for
# the magnitude of the effect -> then proceed to include more fields, and
# potentially varying effects

# interpretation



# diagnostics

