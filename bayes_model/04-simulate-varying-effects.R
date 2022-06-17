library(tidyverse)
library(rethinking)
library(brms)

simulate_data <- function(n, ability_offset = .3, country_offset = .2,
                          institution_offset = 0,
                          indirect_resource_effect = .2,
                          direct_country_effect = .1,
                          direct_institution_effect = .1,
                          true_direct_effect = .4,
                          sd = .4) {
  tibble(
    r_ability = rnorm(n, ability_offset, sd)
  ) %>%
    mutate(r_resources = rnorm(n, r_ability * .4 + country_offset, sd),
           inst_resources = institution_offset + country_offset,
           paper_quality = rnorm(n,
                                 r_ability + r_resources * .2 +
                                   inst_resources * indirect_resource_effect,
                                 sd),
           apc_price = rnorm(n,
                             direct_country_effect + direct_institution_effect +
                               inst_resources * true_direct_effect +
                               paper_quality * .6,
                             sd))
}

# simulate first and last authors with differing effect
set.seed(456)
n <- 80
n_countries <- 5
setup <- tibble(
  country = 1:n_countries,
  n_institutes = rpois(n_countries, 7)
) %>%
  mutate(ability_offset = .3,
         indirect_resource_effect = .0,
         country_offset = rgamma(nrow(.), 2),
         institution_offset = purrr::map(n_institutes, ~runif(.x, max = 5))) %>%
  unnest(institution_offset) %>%
  mutate(n_researchers = rpois(nrow(.), 20 * country_offset),
         inst = seq_along(n_researchers))


sim_first <- setup %>%
  mutate(sim = pmap(list(n = n_researchers, ability_offset = ability_offset,
                         indirect_resource_effect = indirect_resource_effect,
                         country_offset = country_offset,
                         institution_offset = institution_offset), simulate_data)) %>%
  unnest(sim) %>%
  mutate(author_position = "first")


sim_last <- setup %>%
  mutate(sim = pmap(list(n = n_researchers, ability_offset = ability_offset,
                         indirect_resource_effect = indirect_resource_effect,
                         country_offset = country_offset,
                         institution_offset = institution_offset,
                         true_direct_effect = .2), simulate_data)) %>%
  unnest(sim) %>%
  mutate(author_position = "last")

sim <- bind_rows(sim_first, sim_last)
sim


# start with previous model
dlist <- list(
  country = sim$country,
  institute = sim$inst,
  inst_res = sim$inst_resources,
  author_pos = as.numeric(as.factor(sim$author_position)),
  apc_price = sim$apc_price
)

m1 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma + b_r * inst_res,
    b_r ~ dnorm(0, .5),
    z[country] ~ dnorm(0, .5),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, .5),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
precis(m1)
#         mean   sd 5.5% 94.5% n_eff Rhat4
# b_r     0.30 0.01 0.28  0.32  2653     1
# a_sigma 0.18 0.10 0.08  0.36   634     1
# b_bar   0.56 0.10 0.39  0.72   701     1
# b_sigma 0.02 0.02 0.00  0.06  1248     1
# sigma   0.75 0.01 0.73  0.76  4364     1
# as expected, we have an average effect here, because we pooled the coefficients

# try the same with brms
m1.0 <- brm(apc_price ~ 1 + inst_res + (1|country) + (1|institute),
            family = "gaussian",
            prior = c(prior(normal(0, .5), class = Intercept),
                      prior(normal(0, .5), class = b),
                      prior(exponential(1), class = sd),
                      prior(exponential(1), class = sigma)),
            data = dlist,
            warmup = 500, iter = 1000, chains = 4, cores = 4,
            control = list(adapt_delta = 0.95))
summary(m1.0)
conditional_effects(m1.0)
launch_shinystan(m1.0)


# let's add first and last with varying intercept first
m2 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma +
      b_r * inst_res +
      m[author_pos]*c_sigma,
    b_r ~ dnorm(0, .5),
    z[country] ~ dnorm(0, .5),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, .5),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    m[author_pos] ~ dnorm(0, .5),
    c_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma,
    gq> vector[author_pos]:c <<- m*c_sigma
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
precis(m2)
precis(m2, depth = 2)
# the average effect is still the same, but now we have intercepts for "m"
# which are as expected: higher for m[1], negative for m[2], or rather c[1] and
# c[2]

r <- extract.samples(m2)
c_par <- matrix(r$c, ncol = 2)
samples <- c_par %>%
  as_tibble() %>%
  set_names(c("first", "last")) %>%
  mutate(b_r = r$b_r) %>%
  pivot_longer(-b_r, names_to = "author_position",
               values_to = "c_intercept")
samples

samples %>%
  slice_sample(n = 400) %>%
  ggplot() +
  geom_abline(aes(slope = b_r, intercept = c_intercept,
                  colour = author_position),
              alpha = .2, size = 1.1) +
  xlim(0, 2) +
  ylim(-2, 3) +
  theme_bw()
# not very pretty, but we can see that last authors start from a lower intercept

# now to add varying effects
# first let_s rewrite the above model as centered version
m2.1 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_r * inst_res + c[author_pos],
    b_r ~ dnorm(0, .5),
    a[country] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    c[author_pos] ~ dnorm(0, c_sigma),
    c_sigma ~ dexp(1),
    b[institute] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
precis(m2.1)
# still correct estimates, but much less efficient, as expected
precis(m2.1, depth = 2)

# now varying effects
m3 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_auth[author_pos]*inst_res + a_auth[author_pos],

    # varying slope and intercept by author_position
    c(b_auth, a_auth)[author_pos] ~ multi_normal(c(b_r, c), Rho, c_sigma),
    b_r ~ dnorm(0, .5),
    c ~ dnorm(0, .5),
    c_sigma ~ dexp(1),
    Rho ~ lkj_corr(2),

    # varying intercepts as before
    a[country] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),

    b[institute] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),

    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4, log_lik = TRUE
)
precis(m3)
precis(m3, depth = 2)
# sampling was not efficient, but the effect was identified correctly
#             mean   sd  5.5% 94.5% n_eff Rhat4
# a_auth[1]   0.22 0.53 -0.60  1.10   325  1.01
# a_auth[2]   0.07 0.53 -0.75  0.97   323  1.01
# b_auth[1]   0.38 0.03  0.34  0.42   788  1.00 <-
# b_auth[2]   0.20 0.03  0.16  0.24   950  1.00 <-
# b_r         0.23 0.23 -0.18  0.56   923  1.00
# c           0.10 0.46 -0.62  0.85   482  1.01
# c_sigma[1]  0.40 0.39  0.08  1.11  1011  1.01
# c_sigma[2]  0.42 0.53  0.03  1.33   703  1.00
# a[1]       -0.01 0.27 -0.33  0.38   244  1.01
# a[2]        0.05 0.27 -0.27  0.45   244  1.01
# a_sigma     0.30 0.42  0.02  1.03   409  1.00
# b[1]        0.56 0.53 -0.34  1.38   266  1.01
# b[2]        0.57 0.53 -0.33  1.39   266  1.01
# b[3]        0.57 0.53 -0.34  1.38   264  1.01
# b[4]        0.59 0.53 -0.31  1.40   257  1.01
# b[5]        0.59 0.53 -0.32  1.40   268  1.01
# b[6]        0.58 0.52 -0.33  1.39   269  1.01
# b[7]        0.57 0.53 -0.33  1.36   260  1.01
# b[8]        0.56 0.53 -0.34  1.38   262  1.01
# b[9]        0.59 0.53 -0.33  1.40   256  1.01
# b_bar       0.58 0.53 -0.33  1.37   262  1.01
# b_sigma     0.04 0.03  0.01  0.08   221  1.01
# sigma       0.53 0.01  0.51  0.55  1458  1.00

# do the same as above but with brms
m3.1 <- brm(apc_price ~ 1 + inst_res + (1|country) + (1|institute) +
              (1 + inst_res|author_pos),
            family = "gaussian",
            prior = c(prior(normal(0, .5), class = Intercept),
                      prior(normal(0, .5), class = b),
                      prior(exponential(1), class = sd),
                      prior(exponential(1), class = sigma),
                      prior(lkj(2), class = cor)),
            data = dlist,
            warmup = 500, iter = 1000, chains = 4, cores = 4,
            control = list(adapt_delta = .98))
summary(m3.1)
fixef(m3.1)
ranef(m3.1)
# this is correct but not very efficient (many divergent transitions)
# also, the standard errors are quite large. maybe due to small data.
# so increasing data retroactively.

# next step is to vary the intercept also by country
