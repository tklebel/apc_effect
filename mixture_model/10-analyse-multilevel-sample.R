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
# following p126 of mcelreath
sd(mdfi_small$PP_top10)
# 0.04094348
sd(mdfi_small$APC_in_dollar)
# 990.9862

# so b_r of one would mean a change by .04 in pptop10 leads to a change in 990
# in mean APC
# we have b_r = .23 ->
991 * .23
# a change of one sd in pptop10 leads to a change of 228$ in APC, according to
# the model

# compare this to the naive version
lm(apc_price ~ inst_res, data = dlist) %>% summary()
with(dlist, cor(apc_price, inst_res))
# both get us the same value: .229 correlation, or .229 coefficient
# this is weird, it is the exact same value as in the multilevel regression

# doing naive linear regression shows that the coefficient changes
lm(apc_price ~ inst_res + as.factor(country),
   data = dlist) %>%
  summary()

# furthermore, it did not change when we changed the standardisation

# let's try without the institutes
m2 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma + b_bar + b_r * inst_res,
    b_r ~ dnorm(0, 1),
    z[country] ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    b_bar ~ dnorm(0, 1),
    sigma ~ dexp(1),
    gq> vector[country]:a <<- b_bar + z*a_sigma
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
# samples much faster
precis(m2)
# again, the exact same parameter. Can this effect really be that stable?

# run the completely naive model
# naive model
m3 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a + b_r * inst_res,
    a ~ dnorm(0, 1),
    b_r ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
precis(m3)
# again, bloody same .23

# after checking biology, there the parameter changes, but leads to .23 as well
# when including country and institution
# let's find out more about the magnitude
hist(mdfi$PP_top10, breaks = 30)
# so the var goes from 0 to about .25, with an outlier at .32 (mdfi$PP_top10 %>% max())
# the majority of institutions is between .05 and .15. so the parameter roughly
# shows the change from middle to high pptop
#
# what would be interesting here, would be to compare fields, and to compare
# countries, i.e.: have varying effects for b_r within fields and country.
# this would be similar to the correlation plot we have, but incorporating
# country and other institution information, as well as first and last authors
# countries, fields and authors should have varying slopes

# todos:
# - do prior simulations to get meaningful priors
# - read up on varying slopes DONE, but: how to adapt this here? (p448)
#   need to do the sampling and checking first

# diagnostics

