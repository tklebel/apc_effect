library(tidyverse)
library(rethinking)
library(brms)

set.seed(123)

# simulate 200 researcher/paper pairs per institution
n <- 200

simulate_data <- function(n, ability_offset = .3, country_offset = .2,
                          institution_offset = 0,
                          indirect_resource_effect = .2,
                          direct_country_effect = .1,
                          direct_institution_effect = .1,
                          true_direct_effect = .4) {
  tibble(
    r_ability = rnorm(n, ability_offset)
  ) %>%
    mutate(r_resources = rnorm(n, r_ability * .4 + country_offset),
           inst_resources = institution_offset + country_offset,
           paper_quality = rnorm(n, r_ability + r_resources * .2 +
                                   inst_resources * indirect_resource_effect),
           apc_price = rnorm(n, direct_country_effect + direct_institution_effect +
                               inst_resources * true_direct_effect +
                               paper_quality * .6))
}

# this is now the case of a small indirect effect (.2, from inst_res -> paper_quality)
df_c1_i1 <- simulate_data(n, ability_offset = .3, institution_offset = .5)
df_c1_i2 <- simulate_data(n, ability_offset = .3, institution_offset = .7)
df_c2_i3 <- simulate_data(n, ability_offset = .3, institution_offset = .5,
                          country_offset = .2)
df_c2_i4 <- simulate_data(n, ability_offset = .3, institution_offset = .7,
                          country_offset = .2)

df <- tibble(
  country = rep(1:2, each = 2),
  inst = 1:4,
  data = list(df_c1_i1, df_c1_i2, df_c2_i3, df_c2_i4)
)

df <- df %>%
  unnest(data)
df


# model naively
m1 <- lm(apc_price ~ inst_resources, data = df)
summary(m1)

m2 <- lm(apc_price ~ inst_resources + paper_quality, data = df)
summary(m2)

m3 <- lm(apc_price ~ inst_resources + paper_quality + country + inst, data = df)
summary(m3)


# model correctly, with multilevel
dlist <- list(country = df$country,
              institute = df$inst,
              inst_res = df$inst_resources,
              apc_price = df$apc_price)

# start with intercepts only
m4 <- quap(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute],
    a[country] ~ dnorm(0, 1),
    b[institute] ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ), data = dlist
)
precis(m4, depth = 2)

m5 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_r * inst_res,
    a[country] ~ dnorm(0, .5),
    b[institute] ~ dnorm(0, .5),
    b_r ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4
)
precis(m5, depth = 2)


