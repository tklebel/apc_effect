library(tidyverse)
library(rethinking)

set.seed(123)

# simulate 200 researcher/paper pairs per institution
n <- 100

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

m3 <- lm(apc_price ~ inst_resources  + country + inst, data = df)
summary(m3)


# model correctly, with multilevel
dlist <- list(country = df$country,
              institute = df$inst,
              inst_res = df$inst_resources,
              p_qual = df$paper_quality,
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

# next step: simulate different data, with stronger indirect effect
# issue: large intervals: what is the matter here?

# try with zero indirect effect
# this is now the case of a small indirect effect (.2, from inst_res -> paper_quality)
df_c1_i1 <- simulate_data(n, ability_offset = .3, institution_offset = .5, indirect_resource_effect = .01)
df_c1_i2 <- simulate_data(n, ability_offset = .3, institution_offset = .7, indirect_resource_effect = .01)
df_c2_i3 <- simulate_data(n, ability_offset = .3, institution_offset = .5,
                          country_offset = .2, indirect_resource_effect = .01)
df_c2_i4 <- simulate_data(n, ability_offset = .3, institution_offset = .7,
                          country_offset = .2, indirect_resource_effect = .01)

df <- tibble(
  country = rep(1:2, each = 2),
  inst = 1:4,
  data = list(df_c1_i1, df_c1_i2, df_c2_i3, df_c2_i4)
)

df <- df %>%
  unnest(data)
df


# model correctly, with multilevel
dlist <- list(country = df$country,
              institute = df$inst,
              inst_res = df$inst_resources,
              apc_price = df$apc_price)

m6 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_r * inst_res,
    a[country] ~ dnorm(0, .5),
    b[institute] ~ dnorm(0, .5),
    b_r ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4
)
precis(m6, depth = 2)
# we are close to the true effect, but still divergent transitions and a large
# standard error, so the specification might be wrong

# and now with strong biasing effect
indirect_effect = .6
df_c1_i1 <- simulate_data(n, ability_offset = .3, institution_offset = .5,
                          indirect_resource_effect = indirect_effect)
df_c1_i2 <- simulate_data(n, ability_offset = .3, institution_offset = .7,
                          indirect_resource_effect = indirect_effect)
df_c2_i3 <- simulate_data(n, ability_offset = .3, institution_offset = .5,
                          country_offset = .2,
                          indirect_resource_effect = indirect_effect)
df_c2_i4 <- simulate_data(n, ability_offset = .3, institution_offset = .7,
                          country_offset = .2,
                          indirect_resource_effect = indirect_effect)

df <- tibble(
  country = rep(1:2, each = 2),
  inst = 1:4,
  data = list(df_c1_i1, df_c1_i2, df_c2_i3, df_c2_i4)
)

df <- df %>%
  unnest(data)
df


# model correctly, with multilevel
dlist <- list(country = df$country,
              institute = df$inst,
              inst_res = df$inst_resources,
              apc_price = df$apc_price)

m7 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_r * inst_res,
    a[country] ~ dnorm(0, .5),
    b[institute] ~ dnorm(0, .5),
    b_r ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = dlist, chains = 4, cores = 4
)
precis(m7, depth = 2)

# full model with many countries and institutes -----
# estimation should be better with more institutes
# here we do baseline model without bias
set.seed(456)
setup <- tibble(
  country = 1:10,
  n_institutes = rpois(10, 7)
) %>%
  mutate(ability_offset = .3,
         indirect_resource_effect = 0,
         country_offset = rgamma(10, 2),
         institution_offset = purrr::map(n_institutes, ~runif(.x, max = 5))) %>%
  unnest(institution_offset) %>%
  mutate(n_researchers = rpois(nrow(.), 20 * country_offset),
         inst = seq_along(n_researchers))
setup

sim_data <- setup %>%
  mutate(sim = pmap(list(n = n_researchers, ability_offset = ability_offset,
                         indirect_resource_effect = indirect_resource_effect,
                         country_offset = country_offset,
                         institution_offset = institution_offset), simulate_data))

sim_long <- sim_data %>%
  unnest(sim)
sim_long

# check the data
df_check <- distinct(sim_long, country, inst, country_offset, institution_offset,
                     inst_resources)
df_check

dlist_new <- list(
  country = sim_long$country,
  institute = sim_long$inst,
  inst_res = sim_long$inst_resources,
  apc_price = sim_long$apc_price
)

# until now, I have not been doing multilevel modeling correctly,
# see p 403ff of rethinking vol 2
m8 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country],
    a[country] ~ dnorm(a_bar, a_sigma),
    a_bar ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m8, depth = 2)

m9 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- b[institute],
    b[institute] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m9, depth = 2)

m9.1 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute],
    a[country] ~ dnorm(0, a_sigma), # do not have a separate mean parameter here (p416)
    a_sigma ~ dexp(1),
    b[institute] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m9.1, depth = 2)



m10 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a[country] + b[institute] + b_r * inst_res,
    b_r ~ dnorm(0, .5),
    # do not have a separate mean parameter here (p416)
    a[country] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    b[institute] ~ dnorm(b_bar, b_sigma),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m10)
precis(m10, depth = 2)
# this gets us to the true parameter, but it is very inefficient
# need to reparametrize



# reparametrizing (p424)
m10.2 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma + b_r * inst_res,
    b_r ~ dnorm(0, .5),
    z[country] ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, 1),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m10.2)
precis(m10.2, depth = 2)
# this is quite a bit better in terms of rhat


# naive model
m11 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- a + b_r * inst_res,
    a ~ dnorm(0, 1),
    b_r ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dlist_new, chains = 4, cores = 4, log_lik = TRUE
)
precis(m11)

compare(m8, m9, m9.1, m10, m10.2, m11)
# this also shows how the correct model is way better.


# case with strong bias
set.seed(456)
setup_bias <- tibble(
  country = 1:10,
  n_institutes = rpois(10, 7)
) %>%
  mutate(ability_offset = .3,
         indirect_resource_effect = .7,
         country_offset = rgamma(10, 2),
         institution_offset = purrr::map(n_institutes, ~runif(.x, max = 5))) %>%
  unnest(institution_offset) %>%
  mutate(n_researchers = rpois(nrow(.), 20 * country_offset),
         inst = seq_along(n_researchers))


sim_bias <- setup_bias %>%
  mutate(sim = pmap(list(n = n_researchers, ability_offset = ability_offset,
                         indirect_resource_effect = indirect_resource_effect,
                         country_offset = country_offset,
                         institution_offset = institution_offset), simulate_data))

sim_bias <- sim_bias %>%
  unnest(sim)


dlist_bias <- list(
  country = sim_bias$country,
  institute = sim_bias$inst,
  inst_res = sim_bias$inst_resources,
  p_qual = sim_bias$paper_quality,
  apc_price = sim_bias$apc_price
)

m12 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma + b_r * inst_res,
    b_r ~ dnorm(0, .5),
    z[country] ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, 1),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma
  ), data = dlist_bias, chains = 4, cores = 4, log_lik = TRUE
)
precis(m12)

# if we could control for paper quality, this would be solved
# but we can't (right now)
m13 <- ulam(
  alist(
    apc_price ~ dnorm(mu, sigma),
    mu <- z[country]*a_sigma +
      b_bar + x[institute]*b_sigma + b_r * inst_res + b_qual * p_qual,
    b_r ~ dnorm(0, 1),
    b_qual ~ dnorm(0, 1),
    z[country] ~ dnorm(0, 1),
    a_sigma ~ dexp(1),
    x[institute] ~ dnorm(0, 1),
    b_bar ~ dnorm(0, 1),
    b_sigma ~ dexp(1),
    sigma ~ dexp(1),
    gq> vector[institute]:b <<- b_bar + x*b_sigma,
    gq> vector[country]:a <<- z*a_sigma
  ), data = dlist_bias, chains = 4, cores = 4, log_lik = TRUE
)
precis(m13)

# plot the values
png("bayes_model/plots/no_bias.png", width = 400, height = 250)
plot(precis(m10.2))
dev.off()

# do better vis here: visualise the uncertainty in the parameter, and plot the
# true value

Linear_Regression <- extract.samples(m11)$b_r
Multilevel_Model <- extract.samples(m10.2)$b_r
Starker_indirekter_Effekt <- extract.samples(m12)$b_r
Kontrolliert <- extract.samples(m13)$b_r

theme_set(hrbrthemes::theme_ipsum(base_family = "Hind"))
extrafont::loadfonts(device = "win")
# make two plots
p <- tibble(Linear_Regression, Multilevel_Model) %>%
  pivot_longer(everything(), names_to = "model") %>%
  ggplot(aes(value, fill = model)) +
  geom_density(alpha = .7) +
  geom_vline(xintercept = .4) +
  annotate("text", x = .42, y = 65, label = "Wahrer Wert = 0.4") +
  labs(y = NULL, x = "beta-Koeffizient für institutionelle Ressourcen",
       fill = NULL, title = "Posterior samples") +
  theme(legend.position = "top")
ggsave("bayes_model/plots/inst_coef.png", p, width = 7, height = 4.5)

# make two plots
p <- tibble(Starker_indirekter_Effekt, Kontrolliert) %>%
  pivot_longer(everything(), names_to = "model") %>%
  ggplot(aes(value, fill = model)) +
  geom_density(alpha = .7) +
  geom_vline(xintercept = .4) +
  annotate("text", x = .48, y = 65, label = "Wahrer Wert = 0.4") +
  labs(y = NULL, x = "beta-Koeffizient für institutionelle Ressourcen",
       fill = NULL, title = "Posterior samples") +
  theme(legend.position = "top")
ggsave("bayes_model/plots/inst_coef_bias.png", p, width = 7, height = 4.5)
venue

