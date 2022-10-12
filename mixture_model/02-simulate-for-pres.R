library(tidyverse)
library(rethinking)
library(brms)

set.seed(123)

# simulate 1000 researchers
n <- 1000

# start by drawing randomly distributed resources
df <- tibble(
  inst_resources = rnorm(n)
)

# sample institutional prestige as strongly dependent on resources
df <- df %>%
  mutate(inst_prestige = rnorm(n, inst_resources, .5))


# case 1: weak relationship between inst_resources and paper quality
# case 2: strong relationship between inst_resources and paper quality
df <- df %>%
  mutate(paper_quality_case_1 = rnorm(n, inst_resources * .1),
         paper_quality_case_2 = rnorm(n, inst_resources * .6))


# APC price: model with two journals, high and low
df <- df %>%
  mutate(
    apc_high_case_1 = rbernoulli(
      n,
      1/(1 + exp(0 - .5 * inst_resources + .3 * paper_quality_case_1))
    ),
    apc_high_case_2 = rbernoulli(
      n,
      1/(1 + exp(0 - .5 * inst_resources + .3 * paper_quality_case_2))
    )
  )

# model directly with resources
m1.1  <- quap(
  alist(
    apc_high_case_1 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.1)

m1.2  <- quap(
  alist(
    apc_high_case_2 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.2)
# a stronger effect of resources on paper quality leads to bias in measurement

# now let us check out using the proxy directly
m1.3  <- quap(
  alist(
    apc_high_case_1 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_prestige,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.3)

m1.4  <- quap(
  alist(
    apc_high_case_2 ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_prestige,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m1.4)



# do the first version but with brms
m1.5 <- brm(data = df, family = bernoulli(link = "logit"),
            formula = apc_high_case_1 ~ inst_resources)
summary(m1.5)
launch_shinystan(m1.5)


# measurement error model
data_list <- list(inst_prestige = df$inst_prestige,
                  outcome = df$apc_high_case_1,
                  sd = rep(.5, 1000),
                  N = nrow(df))

m2.3 <- brm(data = data_list, family = bernoulli(link = "logit"),
            formula = outcome ~ me(inst_prestige, sd),
            cores = 4, chains = 4,
            save_mevars = TRUE)
summary(m2.3)
# does not work

# new try with mi directly, from examples
bform <- bf(outcome ~ mi(inst_prestige)) +
  bf(inst_prestige | mi(sd) ~ 1) +
  set_rescor(FALSE)

m2.4 <- brm(bform, data = data_list, family = bernoulli(),
            cores = 4, chains = 4)
# mi is not supported for bernoulli models. Maybe model with APC directly?
# drop this: do the full other rest instead


m2.1  <- ulam(
  alist(
    outcome ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    inst_prestige ~ dnorm(inst_resources, sd),
    inst_resources ~ dnorm(0, 1)
  ), data = data_list, chains = 4, cores = 4)
precis(m2.1)

m2.2  <- ulam(
  alist(
    outcome ~ dbinom(1, p),
    logit(p) <- a + br_r*inst_resources,
    inst_resources ~ dnorm(inst_prestige, sigma),
    inst_prestige ~ dnorm(nu, sigma_2),
    sigma_2 ~ dexp(1),
    c(a, nu) ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = data_list, chains = 4, cores = 4)
precis(m2.2)

# plot variables to understand what is going on
df %>%
  ggplot(aes(inst_resources, inst_prestige)) +
  geom_point()
cor(df$inst_resources, df$inst_prestige)
cor(df$inst_prestige, df$apc_high_case_1)
cor(df$inst_resources, df$apc_high_case_1)
