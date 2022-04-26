library(tidyverse)
library(rethinking)

set.seed(123)

# simulate 1000 researchers
n <- 1000

# start by drawing randomly distributed ability
df <- tibble(
  r_ability = rnorm(n)
)

# sample available resources as weakly dependent on ability
df <- df %>%
  mutate(r_resources = rnorm(n, r_ability * .7))


# compute and visualise interdependence
m1  <- quap(
  alist(
    r_resources ~ dnorm(mu, sigma),
    mu <- a + br_a*r_ability,
    a ~ dnorm(0, 1),
    br_a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df)
precis(m1)


df %>%
  ggplot(aes(r_ability, r_resources)) +
  geom_point() +
  geom_abline(intercept = coef(m1)[1], slope = coef(m1)[2])


# simulate paper quality
df <- df %>%
  mutate(paper_quality = rnorm(n, r_ability * .5 + r_resources * .2))

# visualise again
df %>%
  ggplot(aes(paper_quality, r_ability)) +
  geom_point()

df %>%
  ggplot(aes(paper_quality, r_resources)) +
  geom_point()

# compute biased effect of resources on paper quality (without accounting for
# r_ability)
m2.1  <- quap(
  alist(
    paper_quality ~ dnorm(mu, sigma),
    mu <- a + br_r*r_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df)
precis(m2.1)

# now correct for ability
m2.2  <- quap(
  alist(
    paper_quality ~ dnorm(mu, sigma),
    mu <- a + br_r*r_resources + br_a*r_ability,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    br_a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df)
precis(m2.2)


# disregard journal for now, sample oa status directly
# for now model assume no influence of paper quality
df <- df %>%
  mutate(apc_oa = rbernoulli(n, 1/(1 + exp(0 - .5 * r_resources))))
df
# could also think about modelling more papers per researcher, thus using
# a binomial model

# model
m3.1  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
    ), data = df)
precis(m3.1)
exp(.45)
# -> check the above with posterior samples etc. to better understand effect

# and include paper quality for now
df <- df %>%
  mutate(apc_oa = rbernoulli(n, 1/(1 + exp(0 - .5 * r_resources -
                                             .5 * paper_quality))))
df

# model total causal effect
m3.2  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1)
  ), data = df)
precis(m3.2)
exp(.64)
# -> check the above with posterior samples etc. to better understand effect
# total causal effect is more than just direct effect of resources on apc_oa,
# since resources also have effect via paper quality

# model direct effect by accounting for ability
m3.3  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources + br_a*r_ability,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    br_a ~ dnorm(0, 1)
  ), data = df)
precis(m3.3)
# now accounting for ability, we get the direct effect back (albeit still
# somewhat biased)

# use paper quality directly (although not available)
m3.4  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources + b_q*paper_quality,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    b_q ~ dnorm(0, 1)
  ), data = df)
precis(m3.4)
# this is much closer.

# further runs varying the effects to understand how effective controlling is
set.seed(123)
df <- tibble(
  r_ability = rnorm(n)
)

# vary effect of ability on resources
df <- df %>%
  mutate(# strong dependence of resources on ability
         r_resources = rnorm(n, r_ability * .7),
         paper_quality = rnorm(n, r_ability * .5 + r_resources * .2),
         apc_oa = rbernoulli(n, 1/(1 + exp(0 - .5 * r_resources -
                                             .5 * paper_quality))))
m4.1  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources + br_a*r_ability,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    br_a ~ dnorm(0, 1)
  ), data = df)
precis(m4.1)

# weak effect of ability on resources, but stronger effect of resources on paper
# quality
df <- df %>%
  mutate(
    r_resources = rnorm(n, r_ability * .2),
    paper_quality = rnorm(n, r_ability * .2 + r_resources * .5),
    apc_oa = rbernoulli(n, 1/(1 + exp(0 - .5 * r_resources -
                                        .5 * paper_quality))))
m4.2  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources + br_a*r_ability,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    br_a ~ dnorm(0, 1)
  ), data = df)
precis(m4.2)
# in this case, de-confounding does not work well, since the coefficient is not
# the sole direct effect, but the joint direct effect both on paper quality and
# apc oa.
# this can be shown by controlling for paper quality directly, which shows the
# direct effects
m4.3  <- quap(
  alist(
    apc_oa ~ dbinom(1, p),
    logit(p) <- a + br_r*r_resources + b_q*paper_quality,
    a ~ dnorm(0, 1),
    br_r ~ dnorm(0, 1),
    b_q ~ dnorm(0, 1)
  ), data = df)
precis(m4.3)


# next steps:
# - Include journals
# - construct measurement error model for researcher ability
# - check the models' posteriors to get a better feel and understanding for how
# to do this
# - move on to more complicated DAG




